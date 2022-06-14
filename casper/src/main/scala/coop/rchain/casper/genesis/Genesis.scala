package coop.rchain.casper.genesis

import cats.effect.Concurrent
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.BlockRandomSeed
import coop.rchain.casper.genesis.contracts._
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.{RuntimeManager, Tools}
import coop.rchain.casper.util.ProtoUtil.{blockHeader, unsignedBlockProto}
import coop.rchain.casper.util.Sorting.byteArrayOrdering
import coop.rchain.casper.rholang.RuntimeManager.{emptyStateHashFixed, StateHash}
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.{GPrivate, Par}
import coop.rchain.models.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash

final case class Genesis(
    shardId: String,
    blockTimestamp: Long,
    blockNumber: Long,
    proofOfStake: ProofOfStake,
    vaults: Seq[Vault]
)

object Genesis {
  val genesisPubKey = PublicKey(Array[Byte]())

  def NonNegativeMergeableTagName(shardId: String, validatorKey: PublicKey): Par = {
    val nonNegativeContractIndex: Byte = 3
    val rand = BlockRandomSeed
      .generateSplitRandomNumber(
        BlockRandomSeed(
          shardId,
          0,
          validatorKey,
          Blake2b256Hash.fromByteString(emptyStateHashFixed)
        ),
        nonNegativeContractIndex,
        BlockRandomSeed.UserDeploySplitIndex
      )
    val unforgeableByte = Iterator.continually(rand.next()).drop(1).next()
    unforgeableByte.toParUnforgeableName
  }

  // TODO make a hard-coded mainnet unforgeable name after the config of the hard-fork 2 is launched
  def MainnetNonNegativeMergeableTagName: Par = {
    val rand = Blake2b512Random.defaultRandom
    import coop.rchain.models.rholang.implicits._
    rand.next().toParUnforgeableName
  }

  def defaultBlessedTerms(
      posParams: ProofOfStake,
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

  def createGenesisBlock[F[_]: Concurrent: RuntimeManager](genesis: Genesis): F[BlockMessage] = {
    import genesis._

    val blessedTerms = defaultBlessedTerms(proofOfStake, vaults, genesis.shardId)

    RuntimeManager[F]
      .computeGenesis(blessedTerms, blockTimestamp, genesis.blockNumber, genesis.shardId)
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

    val state = RChainState(
      preStateHash = startHash,
      postStateHash = stateHash,
      blockNumber = genesis.blockNumber,
      bonds = bondsProto(proofOfStake).toList
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
    val version = 1L //FIXME make this part of Genesis, and pass it from upstream
    val header  = blockHeader(List.empty[StateHash], version, blockTimestamp)

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
