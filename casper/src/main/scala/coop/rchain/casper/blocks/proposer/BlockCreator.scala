package coop.rchain.casper.blocks.proposer

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.models.syntax._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.merging.ParentsMergedState
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.casper.rholang.sysdeploys.{CloseBlockDeploy, SlashDeploy}
import coop.rchain.casper.rholang.{BlockRandomSeed, InterpreterUtil, RuntimeManager}
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.casper.{MultiParentCasper, PrettyPrinter, ValidatorIdentity}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.crypto.signatures.Signed
import coop.rchain.crypto.PublicKey
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockVersion
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.{Log, Stopwatch, Time}

object BlockCreator {
  private[this] val ProcessDeploysAndCreateBlockMetricsSource =
    Metrics.Source(Metrics.BaseSource, "create-block")

  /*
   * Overview of createBlock
   *
   *  1. Rank each of the block cs's latest messages (blocks) via the LMD GHOST estimator.
   *  2. Let each latest message have a score of 2^(-i) where i is the index of that latest message in the ranking.
   *     Take a subset S of the latest messages such that the sum of scores is the greatest and
   *     none of the blocks in S conflicts with each other. S will become the parents of the
   *     about-to-be-created block.
   *  3. Extract all valid deploys that aren't already in all ancestors of S (the parents).
   *  4. Create a new block that contains the deploys from the previous step.
   */
  def create[F[_]: Concurrent: Time: RuntimeManager: BlockDagStorage: BlockStore: Log: Metrics: Span](
      preState: ParentsMergedState,
      validatorIdentity: ValidatorIdentity,
      shardId: String,
      dummyDeployOpt: Option[(PrivateKey, String)] = None
  ): F[BlockCreatorResult] =
    Span[F].trace(ProcessDeploysAndCreateBlockMetricsSource) {
      val selfId         = ByteString.copyFrom(validatorIdentity.publicKey.bytes)
      val nextSeqNum     = preState.maxSeqNums.get(selfId).map(_ + 1L).getOrElse(0L)
      val nextBlockNum   = preState.maxBlockNum + 1
      val justifications = preState.justifications.map(_.blockHash).toList

      def prepareUserDeploys(blockNumber: Long): F[Set[Signed[DeployData]]] =
        for {
          unfinalized         <- BlockDagStorage[F].pooledDeploys.map(_.values.toSet)
          earliestBlockNumber = blockNumber - MultiParentCasper.deployLifespan
          valid = unfinalized.filter { d =>
            notFutureDeploy(blockNumber, d.data) &&
            notExpiredDeploy(earliestBlockNumber, d.data)
          }
          // this is required to prevent resending the same deploy several times by validator
//          validUnique = valid -- s.deploysInScope
          // TODO: temp solution to filter duplicated deploys
          validUnique <- valid.toList.filterA { d =>
                          BlockDagStorage[F].lookupByDeployId(d.sig).map(_.isEmpty)
                        }
        } yield validUnique.toSet

      def prepareSlashingDeploys(
          ilmFromBonded: Seq[(Validator, BlockHash)],
          rand: Blake2b512Random,
          startIndex: Int
      ): F[List[SlashDeploy]] = {
        val slashingDeploysWithBlocks = ilmFromBonded.zipWithIndex.map {
          case ((slashedValidator, invalidBlock), i) =>
            (SlashDeploy(slashedValidator, rand.splitByte((i + startIndex).toByte)), invalidBlock)
        }
        slashingDeploysWithBlocks.toList.traverse {
          case (sd, invalidBlock) =>
            Log[F]
              .info(
                s"Issuing slashing deploy justified by block ${PrettyPrinter.buildString(invalidBlock)}"
              )
              .as(sd)
        }
      }

      def prepareDummyDeploy(blockNumber: Long, shardId: String): Seq[Signed[DeployData]] =
        dummyDeployOpt match {
          case Some((privateKey, term)) =>
            Seq(
              ConstructDeploy.sourceDeployNow(
                source = term,
                sec = privateKey,
                vabn = blockNumber - 1,
                shardId = shardId
              )
            )
          case None => Seq.empty[Signed[DeployData]]
        }

      val createBlockProcess = for {
        _ <- Log[F].info(
              s"Creating block #${nextBlockNum} (seqNum ${nextSeqNum})"
            )
        userDeploys  <- prepareUserDeploys(nextBlockNum)
        dummyDeploys = prepareDummyDeploy(nextBlockNum, shardId)
        // TODO: fix invalid blocks from non-finalized scope
        ilm <- Seq[(Validator, BlockHash)]().pure[F]
        ilmFromBonded = ilm.filter {
          case (validator, _) => preState.fringeBondsMap.getOrElse(validator, 0L) > 0L
        }
        deploys = userDeploys ++ dummyDeploys
        r <- if (deploys.nonEmpty || ilmFromBonded.nonEmpty) {
              val blockData = BlockData(nextBlockNum, validatorIdentity.publicKey, nextSeqNum)
              val rand = BlockRandomSeed.randomGenerator(
                shardId,
                nextBlockNum,
                validatorIdentity.publicKey,
                preState.preStateHash
              )
              for {
                slashingDeploys <- prepareSlashingDeploys(ilmFromBonded, rand, deploys.size)
                // make sure closeBlock is the last system Deploy
                systemDeploys = slashingDeploys :+ CloseBlockDeploy(
                  rand.splitByte((deploys.size + slashingDeploys.size).toByte)
                )
                preStateHash = preState.preStateHash.toByteString
                checkpointData <- InterpreterUtil.computeDeploysCheckpoint(
                                   deploys.toSeq,
                                   systemDeploys,
                                   rand,
                                   blockData,
                                   preStateHash
                                 )
                (postStateHash, processedDeploys, processedSystemDeploys) = checkpointData

                _ <- Span[F].mark("before-packing-block")

                // Create block and calculate block hash
                unsignedBlock = packageBlock(
                  validatorIdentity.publicKey,
                  blockData,
                  justifications,
                  preStateHash,
                  postStateHash,
                  processedDeploys,
                  processedSystemDeploys,
                  shardId,
                  BlockVersion.Current,
                  // Bonds data in the block is referring to finalized fringe
                  //  not bonds in conflict scope
                  preState.fringeBondsMap,
                  // Rejected data in the block is referring to finalized fringe
                  //  not rejections in conflict scope
                  preState.fringeRejectedDeploys
                )
                _ <- Span[F].mark("block-created")

                // Sign a block (hash should not be changed)
                signedBlock = validatorIdentity.signBlock(unsignedBlock)
                _           <- Span[F].mark("block-signed")

                // This check is temporary until signing function will re-hash the block
                unsignedHash = PrettyPrinter.buildString(unsignedBlock.blockHash)
                signedHash   = PrettyPrinter.buildString(signedBlock.blockHash)
                _ = assert(
                  unsignedBlock.blockHash == signedBlock.blockHash,
                  s"Signed block has different block hash unsigned: $unsignedHash, signed: $signedHash."
                )
              } yield BlockCreatorResult.created(signedBlock)
            } else
              BlockCreatorResult.noNewDeploys.pure[F]
      } yield r

      for {
        // Create block and measure duration
        r                      <- Stopwatch.duration(createBlockProcess)
        (blockStatus, elapsed) = r
        _ <- blockStatus match {
              case Created(block) =>
                val blockInfo   = PrettyPrinter.buildString(block, short = true)
                val deployCount = block.state.deploys.size
                Log[F].info(s"Block created: $blockInfo (${deployCount}d) [$elapsed]")
              case _ => ().pure[F]
            }
      } yield blockStatus
    }

  private def packageBlock(
      sender: PublicKey,
      blockData: BlockData,
      justifications: List[BlockHash],
      preStateHash: StateHash,
      postStateHash: StateHash,
      deploys: Seq[ProcessedDeploy],
      systemDeploys: Seq[ProcessedSystemDeploy],
      shardId: String,
      version: Int,
      // Fringe data
      bondsMap: Map[Validator, Long],
      rejectedDeploys: Set[ByteString]
  ): BlockMessage = {
    val state = RholangState(deploys.toList, systemDeploys.toList)
    ProtoUtil.unsignedBlockProto(
      version,
      shardId,
      blockData.blockNumber,
      sender,
      blockData.seqNum,
      preStateHash,
      postStateHash,
      justifications,
      bondsMap,
      rejectedDeploys,
      state
    )
  }

  private def notExpiredDeploy(earliestBlockNumber: Long, d: DeployData): Boolean =
    d.validAfterBlockNumber > earliestBlockNumber

  private def notFutureDeploy(currentBlockNumber: Long, d: DeployData): Boolean =
    d.validAfterBlockNumber < currentBlockNumber
}
