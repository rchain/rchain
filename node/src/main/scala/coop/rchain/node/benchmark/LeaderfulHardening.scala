package coop.rchain.node.benchmark

import cats.Parallel
import cats.effect.{Concurrent, ContextShift}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.Validator
import coop.rchain.casper.merging.DeployIndex.sysCloseBlockId
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.node.benchmark.utils.GenesisParams.genesisParameters
import coop.rchain.node.benchmark.utils.LeaderfulSimulation.ValidatorWithPayments
import coop.rchain.node.benchmark.utils.{LeaderfulSimulation, Payment, User}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.{Log, Time}
import monix.execution.Scheduler

import java.nio.file.Path
import scala.collection.Seq

object LeaderfulHardening {

  def run[F[_]: Concurrent: Log: Parallel: ContextShift: Time](
      storageDirectory: Path,
      nValidators: Int,
      nUsers: Int,
      maxTxPerBlock: Int,
      epochLength: Int = 10
  )(implicit scheduler: Scheduler): F[Unit] = {
    implicit val m = new Metrics.MetricsNOP[F]
    implicit val s = new NoopSpan[F]

    val users = User.random.take(nUsers).toList
    val validatorsKeys = (1 to nValidators)
      .map(_ => Secp256k1.newKeyPair)
      .map { case (_, pk) => pk }
      .toList

    for {
      rnodeStoreManager <- RNodeKeyValueStoreManager(storageDirectory)
      rSpaceStores      <- rnodeStoreManager.rSpaceStores
      runtimeManager    <- RuntimeManager(rSpaceStores)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      blockDagStorage   <- BlockDagKeyValueStorage.create(rnodeStoreManager)

      _ <- Log[F].info(s"Preparing genesis block...")
      genesis <- {
        val genesisVaults = users.map(_.pk)
        val bondedValidators = validatorsKeys.zipWithIndex
          .map { case (v, i) => Validator(v, (2L * i.toLong + 1L)) }
        Genesis.createGenesisBlock(
          runtimeManager,
          genesisParameters(bondedValidators, genesisVaults, epochLength)
        )
      }
      _ <- Log[F].info(s"Done.")
      _ <- blockStore.put(genesis.blockHash, genesis)
      _ <- blockDagStorage.insert(genesis, invalid = false, approved = true)
      layers = {
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

        LeaderfulSimulation.go(
          genesis,
          validatorsWithPayments,
          initBalances,
          epochLength,
          Int.MaxValue
        )
      }
      _ <- layers.compile.lastOrError
    } yield ()
  }
}
