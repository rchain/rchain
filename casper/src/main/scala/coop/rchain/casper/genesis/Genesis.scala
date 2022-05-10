package coop.rchain.casper.genesis

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.casper.rholang.{RuntimeManager, Tools}
import coop.rchain.casper.util.ProtoUtil.unsignedBlockProto
import coop.rchain.casper.util.Sorting.byteArrayOrdering
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.{GPrivate, Par}

final case class Genesis(
    sender: PublicKey,
    shardId: String,
    blockTimestamp: Long,
    blockNumber: Long,
    proofOfStake: ProofOfStake,
    registry: Registry,
    vaults: Seq[Vault]
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
      posParams: ProofOfStake,
      registry: Registry,
      vaults: Seq[Vault],
      shardId: String
  ): Seq[Signed[DeployData]] = {
    // Splits initial vaults creation in multiple deploys (batches)
    val vaultBatches = vaults.grouped(100).toSeq
    val vaultDeploys = vaultBatches.zipWithIndex.map {
      case (batchVaults, idx) =>
        StandardDeploys.revGenerator(
          batchVaults,
          timestamp = 1565818101792L + idx,
          isLastBatch = 1 + idx == vaultBatches.size,
          shardId
        )
    }

    // Order of deploys is important for Registry to work correctly
    // - dependencies must be defined first in the list
    StandardDeploys.registryGenerator(registry, shardId) +:
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

  def createGenesisBlock[F[_]: Concurrent: RuntimeManager](genesis: Genesis): F[BlockMessage] = {
    val blessedTerms =
      defaultBlessedTerms(genesis.proofOfStake, genesis.registry, genesis.vaults, genesis.shardId)

    RuntimeManager[F]
      .computeGenesis(blessedTerms, genesis.blockTimestamp, genesis.blockNumber, genesis.sender)
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
    val state = RChainState(
      preStateHash = startHash,
      postStateHash = stateHash,
      blockNumber = genesis.blockNumber,
      bonds = bondsProto(genesis.proofOfStake).toList
    )

    //FIXME any failures here should terminate the genesis ceremony
    val blockDeploys = processedDeploys.filterNot(_.isFailed)
    val sortedDeploys =
      blockDeploys.map(d => d.copy(deployLog = d.deployLog.sortBy(_.toProto.toByteArray)))
    val body = Body(
      state = state,
      deploys = sortedDeploys.toList,
      rejectedDeploys = List.empty,
      systemDeploys = List.empty
    )
    val version = 1 //FIXME make this part of Genesis, and pass it from upstream
    val seqNum  = 0L
    val header  = Header(List.empty[StateHash], genesis.blockTimestamp)

    unsignedBlockProto(version, genesis.sender, body, header, List.empty, genesis.shardId, seqNum)
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
