package coop.rchain.casper.blocks.proposer

import cats.effect.{Async, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.merging.ParentsMergedState
import coop.rchain.casper.protocol.{ProcessedDeploy, ProcessedSystemDeploy, RholangState}
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.casper.rholang.sysdeploys.{CloseBlockDeploy, SlashDeploy}
import coop.rchain.casper.rholang.{BlockRandomSeed, InterpreterUtil, RuntimeManager}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.{PrettyPrinter, ValidatorIdentity}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockVersion
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.shared.Log

final case class BlockCreator(id: ValidatorIdentity, shardId: String) {
  type StateTransitionResult = (StateHash, Seq[ProcessedDeploy], Seq[ProcessedSystemDeploy])

  def create[F[_]: Async: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      preState: ParentsMergedState,
      deploys: Seq[DeployId],
      toSlash: Set[Validator] = Set.empty,
      changeEpoch: Boolean = false,
      suppressAttestation: Boolean = true
  ): F[BlockCreatorResult] = {
    val preStateHash      = preState.preStateHash
    val parents           = preState.justifications.map(_.blockHash)
    val bondsMap          = preState.fringeBondsMap
    val blockNum          = preState.justifications.map(_.blockNum).max + 1
    val creatorsPk        = id.publicKey
    val creatorsId        = creatorsPk.bytes.toByteString
    val creatorsLatestOpt = preState.justifications.find(_.sender == creatorsId)
    val seqNum            = creatorsLatestOpt.map(_.seqNum + 1).getOrElse(0L)
    val blockData         = BlockData(blockNum, creatorsPk, seqNum)
    val shouldPropose     = deploys.nonEmpty || toSlash.nonEmpty || changeEpoch

    // deploys that are rejected on finalization done by the block being created
    val finalization = preState.fringeRejectedDeploys

    def propose: F[StateTransitionResult] = {
      val rand = BlockRandomSeed.randomGenerator(shardId, blockNum, creatorsPk, preStateHash)
      // seeds from 0 to deploys.size are used in deploys execution, so system deploy seeds start from the next index
      val slashSeeds =
        (0 until toSlash.size).map(_ + deploys.size).map(i => rand.splitByte(i.toByte))
      val closeSeed = rand.splitByte((deploys.size + toSlash.size).toByte)

      val slashDeploys = toSlash.toList.sorted.zip(slashSeeds).map(SlashDeploy.tupled)
      val closeDeploy  = CloseBlockDeploy(closeSeed)

      BlockDagStorage[F].pooledDeploys
        .map(_.filterKeys(deploys.toSet).values.toSeq)
        .flatMap(
          InterpreterUtil.computeDeploysCheckpoint[F](
            _,
            slashDeploys :+ closeDeploy,
            rand,
            blockData,
            preStateHash.toByteString
          )
        )
    }

    /** Create attestation. */
    def attest: F[StateTransitionResult] = Sync[F].delay {
      val postStateHash          = preStateHash.toByteString
      val processedDeploys       = Seq()
      val processedSystemDeploys = Seq()
      (postStateHash, processedDeploys, processedSystemDeploys)
    }

    val postState =
      if (shouldPropose) propose.map(_.some)
      else (!suppressAttestation).guard[Option].traverse(_ => attest)

    postState.map {
      case None                                                            => BlockCreatorResult.noNewDeploys
      case Some((postStateHash, processedDeploys, processedSystemDeploys)) =>
        // Create block and calculate block hash
        val state = RholangState(processedDeploys.toList, processedSystemDeploys.toList)
        val unsignedBlock = ProtoUtil.unsignedBlockProto(
          version = BlockVersion.Current,
          shardId,
          blockData.blockNumber,
          creatorsPk,
          blockData.seqNum,
          preStateHash.toByteString,
          postStateHash,
          parents.toList,
          bondsMap,
          finalization,
          state
        )

        // Sign a block (hash should not be changed)
        val signedBlock = id.signBlock(unsignedBlock)

        // This check is temporary until signing function will re-hash the block
        val unsignedHash = PrettyPrinter.buildString(unsignedBlock.blockHash)
        val signedHash   = PrettyPrinter.buildString(signedBlock.blockHash)
        assert(
          unsignedBlock.blockHash == signedBlock.blockHash,
          s"Signed block has different block hash unsigned: $unsignedHash, signed: $signedHash."
        )
        BlockCreatorResult.created(signedBlock)
    }
  }
}
