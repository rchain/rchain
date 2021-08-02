package coop.rchain.node.benchmark

import cats.Parallel
import cats.effect.{Concurrent, ContextShift}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Validator, Vault}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.node.benchmark.DagBuilder.ValidatorWithPayments
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.syntax._
import coop.rchain.shared.{Log, Time}
import monix.execution.Scheduler

import java.nio.file.Path
import scala.collection.Seq

object Leadefrful {

  val predefinedVaultsAmt = 900000000L

  def genesisParameters(
      bondedValidators: Seq[Validator],
      genesisVaults: List[PublicKey]
  ): Genesis = {
    def predefinedVault(pub: PublicKey): Vault =
      Vault(RevAddress.fromPublicKey(pub).get, predefinedVaultsAmt)

    Genesis(
      shardId = "root",
      timestamp = 0L,
      proofOfStake = ProofOfStake(
        minimumBond = 0L,
        maximumBond = Long.MaxValue,
        // Epoch length is set to large number to prevent trigger of epoch change
        // in PoS close block method, which causes block merge conflicts
        // - epoch change can be set as a parameter in Rholang tests (e.g. PoSSpec)
        epochLength = 1000,
        quarantineLength = 50000,
        numberOfActiveValidators = 100,
        validators = bondedValidators
      ),
      vaults = genesisVaults.map(predefinedVault) ++
        bondedValidators.toList.map {
          case Validator(pk, _) =>
            // Initial validator vaults contain 0 Rev
            RevAddress.fromPublicKey(pk).map(Vault(_, 0))
        }.flattenOption,
      supply = Long.MaxValue,
      blockNumber = 0
    )
  }

  def run[F[_]: Concurrent: Log: Parallel: Span: Metrics: ContextShift: Time](
      storageDirectory: Path,
      nValidators: Int,
      nUsers: Int,
      maxTxPerBlock: Int
  )(implicit scheduler: Scheduler): F[Unit] = {

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
          genesisParameters(bondedValidators, genesisVaults)
        )
      }
      _ <- Log[F].info(s"Done.")
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

        DagBuilder.leaderful(genesis, validatorsWithPayments, initBalances, 100000)
      }
    } yield ()
  }
}
