package coop.rchain.node.hardening

import cats.effect.Concurrent
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.Validator
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.GenesisBuilder
import coop.rchain.casper.util.rholang.{Resources, RuntimeManager}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.node.benchmark.DagBuilder.ValidatorWithPayments
import coop.rchain.node.benchmark.Leadefrful.genesisParameters
import coop.rchain.node.benchmark._
import coop.rchain.node.effects
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rspace.syntax._
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.shared.{Log, Time}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import java.nio.file.Files
import scala.collection.Seq

class Hardening extends FlatSpec with Matchers {

  "multiple tx in single block" should "be merged correctly" in effectTest {

    val nUsers      = 12
    val nValidators = 2
    val dataDir     = Files.createTempDirectory(s"hash-set-casper-test-genesis-")

    implicit val c                                 = Concurrent[Task]
    implicit val metrics: Metrics.MetricsNOP[Task] = new Metrics.MetricsNOP
    implicit val logEff                            = Log.log[Task]
    implicit val time: Time[Task]                  = effects.time
    implicit val span                              = new NoopSpan[Task]

    val users = User.random.take(nUsers).toList
    val validatorsKeys = (1 to nValidators)
      .map(_ => Secp256k1.newKeyPair)
      .map { case (_, pk) => pk }
      .toList

    for {
      rnodeStoreManager <- RNodeKeyValueStoreManager(dataDir)
      rSpaceStores      <- rnodeStoreManager.rSpaceStores
      runtimeManager    <- RuntimeManager(rSpaceStores)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      blockDagStorage   <- BlockDagKeyValueStorage.create(rnodeStoreManager)

      genesis <- {
        val genesisVaults = users.map(_.pk)
        val bondedValidators = validatorsKeys.zipWithIndex
          .map { case (v, i) => Validator(v, (2L * i.toLong + 1L)) }
        Genesis.createGenesisBlock(
          runtimeManager,
          genesisParameters(bondedValidators, genesisVaults)
        )
      }
      _ <- blockStore.put(genesis.blockHash, genesis)
      _ <- blockDagStorage.insert(genesis, invalid = false, approved = true)
      _ <- {
        implicit val rm  = runtimeManager
        implicit val bds = blockDagStorage

        val payments = {
//          val Seq(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11) = (1 to 11).map(users(_))
          val u1 = users(0)
          Iterator
            .continually(
              List(
                List(
                  Payment(u1, u1, 1000)
                ),
                List(Payment(u1, u1, 16))
              )
            )
            .flatten
        }

        val validatorsWithPayments = Iterator
          .continually(validatorsKeys)
          .flatten
          .zip(payments)
          .map(ValidatorWithPayments.tupled)
          .grouped(validatorsKeys.size)

        val initUserBalances = users.map {
          case User(_, pk, _) =>
            (users.find(_.pk == pk).get, (900000000L, Seq.empty[Payment]))
        }.toMap
        val validatorUsers = validatorsKeys.map { pk =>
          User(PrivateKey(ByteString.EMPTY), pk, Base16.encode(pk.bytes))
        }
        val initValidatorBalances = validatorUsers.map(_ -> (0L, List.empty)).toMap
        val initBalances          = initUserBalances ++ initValidatorBalances

        DagBuilder.leaderful(genesis, validatorsWithPayments, initBalances, 5)
      }
    } yield ()

  }

  "merging with leader" should "work" in effectTest {

    val nUsers        = 20
    val nValidators   = 4
    val maxTxPerBlock = 1
    val mergesNum     = 10
    val dataDir       = Files.createTempDirectory(s"hash-set-casper-test-genesis-")

    implicit val c                                 = Concurrent[Task]
    implicit val metrics: Metrics.MetricsNOP[Task] = new Metrics.MetricsNOP
    implicit val logEff                            = Log.log[Task]
    implicit val time: Time[Task]                  = effects.time
    implicit val span                              = new NoopSpan[Task]

    val users = User.random.take(nUsers).toList
    val validatorsKeys = (1 to nValidators)
      .map(_ => Secp256k1.newKeyPair)
      .map { case (_, pk) => pk }
      .toList

    for {
      rnodeStoreManager <- RNodeKeyValueStoreManager(dataDir)
      rSpaceStores      <- rnodeStoreManager.rSpaceStores
      runtimeManager    <- RuntimeManager(rSpaceStores)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      blockDagStorage   <- BlockDagKeyValueStorage.create(rnodeStoreManager)

      genesis <- {
        val genesisVaults = users.map(_.pk)
        val bondedValidators = validatorsKeys.zipWithIndex
          .map { case (v, i) => Validator(v, (2L * i.toLong + 1L)) }
        Genesis.createGenesisBlock(
          runtimeManager,
          genesisParameters(bondedValidators, genesisVaults)
        )
      }
      _ <- blockStore.put(genesis.blockHash, genesis)
      _ <- blockDagStorage.insert(genesis, invalid = false, approved = true)
      _ <- {
        implicit val rm  = runtimeManager
        implicit val bds = blockDagStorage

        val payments = Payment.randomBatches(users, 1, 10, maxTxPerBlock)

        val validatorsWithPayments = Iterator
          .continually(validatorsKeys)
          .flatten
          .zip(payments)
          .map(ValidatorWithPayments.tupled)
          .grouped(validatorsKeys.size)

        val initUserBalances = users.map {
          case User(_, pk, _) =>
            (users.find(_.pk == pk).get, (900000000L, Seq.empty[Payment]))
        }.toMap
        val validatorUsers = validatorsKeys.map { pk =>
          User(PrivateKey(ByteString.EMPTY), pk, Base16.encode(pk.bytes))
        }
        val initValidatorBalances = validatorUsers.map(_ -> (0L, List.empty)).toMap
        val initBalances          = initUserBalances ++ initValidatorBalances

        DagBuilder.leaderful(genesis, validatorsWithPayments, initBalances, mergesNum)
      }
    } yield ()

  }
}
