package coop.rchain.node.hardening

import cats.Monad
import cats.effect.Concurrent
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.Validator
import coop.rchain.casper.merging.DeployIndex._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.node.benchmark.utils.GenesisParams.genesisParameters
import coop.rchain.node.benchmark.utils.LeaderfulSimulation.ValidatorWithPayments
import coop.rchain.node.benchmark.utils.Payment.BalanceSheet
import coop.rchain.node.benchmark.utils.{LeaderfulSimulation, Payment, User}
import coop.rchain.node.effects
import coop.rchain.rspace.syntax._
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.shared.{Log, Time}
import coop.rchain.store.InMemoryStoreManager
import fs2.Pipe
import fs2.Stream
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.Seq

class Hardening extends FlatSpec with Matchers {

  "leaderful DAG" should "be ok" in effectTest {

    val nUsers        = 20
    val nValidators   = 4
    val maxTxPerBlock = 1

    val users = User.random.take(nUsers).toList
    val validators = (1 to nValidators)
      .map(_ => Secp256k1.newKeyPair)
      .map { case (_, pk) => pk }
      .toList
    val payments = Payment.randomBatches(users, 1, 10, maxTxPerBlock)

    val epochLength = 3

    /**
      *    * 10 -> 2 of 3 closeBlock should be rejected
      * ***  9  -> epoch change
      *    * 8
      * ***  7
      *    * 6  -> epoch change, no closeBlock should be rejected
      * ***  5
      *    * 4  -> 2 of 3 closeBlock should be rejected
      * ***  3  -> epoch change
      *    * 2  -> no closeBlock should be rejected
      * ***  1
      * GGGG 0
      */
    val validatorsWithPayments = Iterator
      .continually(validators)
      .flatten
      .zip(payments)
      .map(ValidatorWithPayments.tupled)
      .grouped(validators.size)

    implicit val c                                 = Concurrent[Task]
    implicit val metrics: Metrics.MetricsNOP[Task] = new Metrics.MetricsNOP
    implicit val logEff                            = Log.log[Task]
    implicit val time: Time[Task]                  = effects.time
    implicit val span                              = new NoopSpan[Task]

    // run everything in memory
    val rnodeStoreManager = new InMemoryStoreManager

    for {
      rSpaceStores    <- rnodeStoreManager.rSpaceStores
      runtimeManager  <- RuntimeManager(rSpaceStores)
      blockStore      <- KeyValueBlockStore(rnodeStoreManager)
      blockDagStorage <- BlockDagKeyValueStorage.create(rnodeStoreManager)
      r <- {
        val genesisVaults = users.map(_.pk)
        val bondedValidators = validators.zipWithIndex
          .map { case (v, i) => Validator(v, (2L * i.toLong + 1L)) }

        Genesis
          .createGenesisBlock(
            runtimeManager,
            genesisParameters(bondedValidators, genesisVaults, epochLength = epochLength)
          )
          .map((_, users, validators))
      }
      (genesis, users, validators) = r
      _                            <- blockStore.put(genesis.blockHash, genesis)
      _                            <- blockDagStorage.insert(genesis, invalid = false, approved = true)
      layers = {
        implicit val rm  = runtimeManager
        implicit val bds = blockDagStorage

        val initUserBalances = users.map {
          case User(_, pk, _) =>
            (users.find(_.pk == pk).get, (900000000L, Seq.empty[Payment]))
        }.toMap
        val validatorUsers = validators.map { pk =>
          User(PrivateKey(ByteString.EMPTY), pk, Base16.encode(pk.bytes))
        }
        val initValidatorBalances = validatorUsers.map(_ -> (0L, List.empty)).toMap
        val initBalances          = initUserBalances ++ initValidatorBalances

        LeaderfulSimulation
          .go(genesis, validatorsWithPayments, initBalances, 3, 5)
      }
      _ <- layers.compile.lastOrError
    } yield ()
  }
}
