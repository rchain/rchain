package coop.rchain.casper.genesis

import java.nio.file.Path

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, unsignedBlockProto}
import coop.rchain.casper.util.Sorting.byteArrayOrdering
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.{InternalProcessedDeploy, RuntimeManager}
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.shared.{Log, LogSource, Time}

final case class Genesis(
    shardId: String,
    timestamp: Long,
    proofOfStake: ProofOfStake,
    vaults: Seq[Vault],
    supply: Long
)

object Genesis {

  def defaultBlessedTerms(
      timestamp: Long,
      posParams: ProofOfStake,
      vaults: Seq[Vault],
      supply: Long
  ): Seq[DeployData] =
    Seq(
      StandardDeploys.registry,
      StandardDeploys.listOps,
      StandardDeploys.either,
      StandardDeploys.nonNegativeNumber,
      StandardDeploys.makeMint,
      StandardDeploys.authKey,
      StandardDeploys.revVault,
      StandardDeploys.revGenerator(vaults, supply),
      StandardDeploys.poSGenerator(posParams),
      StandardDeploys.treeHashMap
    )

  def createGenesisBlock[F[_]: Concurrent](
      runtimeManager: RuntimeManager[F],
      genesis: Genesis
  ): F[BlockMessage] = {
    import genesis._

    val blessedTerms = defaultBlessedTerms(
      timestamp,
      proofOfStake,
      vaults,
      supply = Long.MaxValue
    )

    runtimeManager
      .computeGenesis(blessedTerms, timestamp)
      .map {
        case (startHash, stateHash, processedDeploys) =>
          createInternalProcessedDeploy(genesis, startHash, stateHash, processedDeploys)
      }
  }

  private def createInternalProcessedDeploy(
      genesis: Genesis,
      startHash: StateHash,
      stateHash: StateHash,
      processedDeploys: Seq[InternalProcessedDeploy]
  ): BlockMessage = {
    import genesis._

    val state = RChainState(
      preStateHash = startHash,
      postStateHash = stateHash,
      blockNumber = 0,
      bonds = bondsProto(proofOfStake).toList
    )

    val blockDeploys =
      processedDeploys.filterNot(_.status.isFailed).map(_.toProcessedDeploy)
    val sortedDeploys = blockDeploys.map(
      d =>
        d.copy(
          deployLog = d.deployLog.sortBy(_.toProto.toByteArray),
          paymentLog = d.paymentLog.sortBy(_.toProto.toByteArray)
        )
    )

    val body    = Body(state = state, deploys = sortedDeploys.toList)
    val version = 1L //FIXME make this part of Genesis, and pass it from upstream
    val header  = blockHeader(body, List.empty[StateHash], version, timestamp)

    unsignedBlockProto(body, header, List.empty[Justification], shardId)
  }

  private def bondsProto(proofOfStake: ProofOfStake): Seq[Bond] = {
    val bonds = proofOfStake.validators.flatMap(Validator.unapply).toMap
    import coop.rchain.crypto.util.Sorting.publicKeyOrdering
    //sort to have deterministic order (to get reproducible hash)
    bonds.toIndexedSeq.sorted.map {
      case (pk, stake) =>
        val validator = ByteString.copyFrom(pk.bytes)
        Bond(validator, stake)
    }
  }
}
