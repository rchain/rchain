package coop.rchain.casper.genesis

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil.{blockHeader, unsignedBlockProto}
import coop.rchain.casper.util.Sorting.byteArrayOrdering
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.{RuntimeManager, Tools}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.{GPrivate, Par}

final case class Genesis(
    shardId: String,
    timestamp: Long,
    blockNumber: Long,
    proofOfStake: ProofOfStake,
    vaults: Seq[Vault],
    supply: Long
)

object Genesis {

  val NonNegativeMergeableTagName: Par = {
    val rand = Tools.unforgeableNameRng(
      StandardDeploys.nonNegativeNumberPubKey,
      StandardDeploys.nonNegativeNumberTimestamp
    )
    import coop.rchain.models.rholang.implicits._
    val unforgeableByte = Iterator.continually(rand.next()).drop(1).next()
    GPrivate(ByteString.copyFrom(unforgeableByte))
  }

  def defaultBlessedTerms(
      timestamp: Long,
      posParams: ProofOfStake,
      vaults: Seq[Vault],
      supply: Long,
      shardId: String
  ): Seq[Signed[DeployData]] = {
    // Splits initial vaults creation in multiple deploys (batches)
    val vaultBatches = vaults.grouped(100).toSeq
    val vaultDeploys = vaultBatches.zipWithIndex.map {
      case (batchVaults, idx) =>
        StandardDeploys.revGenerator(
          batchVaults,
          supply,
          timestamp = 1565818101792L + idx,
          isLastBatch = 1 + idx == vaultBatches.size,
          shardId
        )
    }

    // Order of deploys is important for Registry to work correctly
    // - dependencies must be defined first in the list
    StandardDeploys.registry(shardId) +:
      StandardDeploys.listOps(shardId) +:
      StandardDeploys.either(shardId) +:
      StandardDeploys.nonNegativeNumber(shardId) +:
      StandardDeploys.makeMint(shardId) +:
      StandardDeploys.authKey(shardId) +:
      StandardDeploys.revVault(shardId) +:
      StandardDeploys.multiSigRevVault(shardId) +:
      vaultDeploys :+
      StandardDeploys.poSGenerator(posParams, shardId)
  }

  def createGenesisBlock[F[_]: Concurrent](
      runtimeManager: RuntimeManager[F],
      genesis: Genesis
  ): F[BlockMessage] = {
    import genesis._

    val blessedTerms = defaultBlessedTerms(
      timestamp,
      proofOfStake,
      vaults,
      supply = Long.MaxValue,
      shardId = genesis.shardId
    )

    runtimeManager
      .computeGenesis(blessedTerms, timestamp, genesis.blockNumber)
      .map {
        case (startHash, stateHash, processedDeploys) =>
          createProcessedDeploy(genesis, startHash, stateHash, processedDeploys)
      }
  }

  private def createProcessedDeploy(
      genesis: Genesis,
      startHash: StateHash,
      stateHash: StateHash,
      processedDeploys: Seq[ProcessedDeploy]
  ): BlockMessage = {
    import genesis._

    //FIXME any failures here should terminate the genesis ceremony
    val blockDeploys = processedDeploys.filterNot(_.isFailed)
    val sortedDeploys =
      blockDeploys.map(d => d.copy(deployLog = d.deployLog.sortBy(_.toProto.toByteArray)))
    val state = RholangTrace(
      deploys = sortedDeploys.toList,
      rejectedDeploys = List.empty,
      systemDeploys = List.empty
    )
    val version       = 1L //FIXME make this part of Genesis, and pass it from upstream
    val header        = blockHeader(List.empty[StateHash], version, timestamp)
    val bonds         = bondsProto(proofOfStake).toList
    val preStateHash  = startHash
    val postStateHash = stateHash
    val blockNumber   = genesis.blockNumber

    unsignedBlockProto(
      state,
      preStateHash,
      postStateHash,
      bonds,
      header,
      List.empty[Justification],
      shardId,
      blockNumber
    )
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
