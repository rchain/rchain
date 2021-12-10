package coop.rchain.casper.blocks.proposer

import cats.effect.Concurrent
import cats.instances.list._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.protocol.{Header, _}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.util.rholang.costacc.{CloseBlockDeploy, SlashDeploy}
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.casper.{Casper, CasperSnapshot, PrettyPrinter, ValidatorIdentity}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.DeployId
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
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
  def create[F[_]: Concurrent: Log: Time: BlockStore: DeployStorage: Metrics: RuntimeManager: Span](
      s: CasperSnapshot[F],
      validatorIdentity: ValidatorIdentity,
      dummyDeployOpt: Option[(PrivateKey, String)] = None
  )(implicit runtimeManager: RuntimeManager[F]): F[BlockCreatorResult] =
    Span[F].trace(ProcessDeploysAndCreateBlockMetricsSource) {
      val selfId         = ByteString.copyFrom(validatorIdentity.publicKey.bytes)
      val nextSeqNum     = s.maxSeqNums.getOrElse(selfId, 0) + 1
      val nextBlockNum   = s.maxBlockNum + 1
      val justifications = s.latestMessages

      def prepareUserDeploys(blockNumber: Long): F[Set[Signed[DeployData]]] =
        for {
          unfinalized         <- DeployStorage[F].readAll
          earliestBlockNumber = blockNumber - s.deployLifespan
          valid = unfinalized.filter(
            d =>
              notFutureDeploy(blockNumber, d.data) &&
                notExpiredDeploy(earliestBlockNumber, d.data)
          )
          // this is required to prevent resending the same deploy several times by validator
          validUnique = valid.filterNot(
            d => s.deploysInScope.contains(d.sig)
          )
        } yield validUnique.take(1)

      def prepareSlashingDeploys(
          seqNum: Int,
          activeValidators: Set[Validator]
      ): F[Seq[SlashDeploy]] =
        for {
          bondedOffenders <- Casper.bondedOffenders(s, activeValidators)
          // TODO: Add `slashingDeploys` to DeployStorage
          slashingDeploys = bondedOffenders
            .map(_._2)
            .map(
              invalidBlockHash =>
                SlashDeploy(
                  invalidBlockHash,
                  validatorIdentity.publicKey,
                  SystemDeployUtil.generateSlashDeployRandomSeed(selfId, seqNum)
                )
            )
            .toList
          _ <- slashingDeploys.traverse_(
                sd =>
                  Log[F].info(
                    s"Issuing slashing deploy justified by block ${PrettyPrinter.buildString(sd.invalidBlockHash)}"
                  )
              )
        } yield slashingDeploys

      def prepareDummyDeploy(blockNumber: Long): Seq[Signed[DeployData]] = dummyDeployOpt match {
        case Some((privateKey, term)) =>
          Seq(
            ConstructDeploy.sourceDeployNow(
              source = term,
              sec = privateKey,
              vabn = blockNumber - 1
            )
          )
        case None => Seq.empty[Signed[DeployData]]
      }

      val createBlockProcess = for {
        _                <- Log[F].info(s"Creating block #${nextBlockNum} (seqNum ${nextSeqNum})")
        preStateHash     = s.finalizedFringe.state
        activeValidators <- runtimeManager.getActiveValidators(preStateHash)

        userDeploys     <- prepareUserDeploys(nextBlockNum)
        dummyDeploys    = prepareDummyDeploy(nextBlockNum)
        slashingDeploys <- prepareSlashingDeploys(nextSeqNum, activeValidators.toSet)
        // make sure closeBlock is the last system Deploy
        systemDeploys = slashingDeploys :+ CloseBlockDeploy(
          SystemDeployUtil
            .generateCloseDeployRandomSeed(selfId, nextSeqNum)
        )
        deploys = if (userDeploys.isEmpty) dummyDeploys else userDeploys

        now           <- Time[F].currentMillis
        invalidBlocks = s.invalidBlocks
        blockData     = BlockData(now, nextBlockNum, validatorIdentity.publicKey, nextSeqNum)
        checkpointData <- InterpreterUtil.computeDeploysCheckpoint(
                           deploys.toSeq,
                           systemDeploys,
                           runtimeManager,
                           blockData,
                           invalidBlocks,
                           preStateHash
                         )
        (
          postStateHash,
          processedDeploys,
          processedSystemDeploys
        )             = checkpointData
        newBonds      <- runtimeManager.computeBonds(postStateHash)
        _             <- Span[F].mark("before-packing-block")
        shardId       = s.shardName
        casperVersion = s.casperVersion
        // unsignedBlock got blockHash(hashed without signature)
        unsignedBlock = packageBlock(
          blockData,
          justifications.toList.map { case (s, m) => Justification(s, m.blockHash) },
          preStateHash,
          postStateHash,
          processedDeploys,
          List.empty[ByteString],
          processedSystemDeploys,
          newBonds,
          shardId,
          casperVersion
        )
        _ <- Span[F].mark("block-created")
        // signedBlock add signature and replace hashed-without-signature
        // blockHash to hashed-with-signature blockHash
        signedBlock = validatorIdentity.signBlock(unsignedBlock)
        _           <- Span[F].mark("block-signed")

        // TODO this is tempt solution to remove deploys from deploy storage once they put in a block
        _ <- DeployStorage[F].remove(userDeploys.toList)
      } yield BlockCreatorResult.created(signedBlock)

      for {
        // Create block and measure duration
        r                      <- Stopwatch.duration(createBlockProcess)
        (blockStatus, elapsed) = r
        _ <- blockStatus match {
              case Created(block) =>
                val blockInfo   = PrettyPrinter.buildString(block, short = true)
                val deployCount = block.body.deploys.size
                Log[F].info(s"Block created: $blockInfo (${deployCount}d) [$elapsed]")
              case _ => ().pure[F]
            }
      } yield blockStatus
    }

  private def packageBlock(
      blockData: BlockData,
      justifications: Seq[Justification],
      preStateHash: StateHash,
      postStateHash: StateHash,
      deploys: Seq[ProcessedDeploy],
      rejectedDeploys: Seq[ByteString],
      systemDeploys: Seq[ProcessedSystemDeploy],
      bondsMap: Seq[Bond],
      shardId: String,
      version: Long
  ): BlockMessage = {
    val state = RChainState(preStateHash, postStateHash, bondsMap.toList, blockData.blockNumber)
    val body = {
      Body(
        state,
        deploys.toList,
        rejectedDeploys.map(r => RejectedDeploy(r)).toList,
        systemDeploys.toList
      )
    }
    val parents = justifications.map(_.latestBlockHash).toList
    val header  = Header(parents, blockData.timeStamp, version)
    ProtoUtil.unsignedBlockProto(body, header, justifications, shardId, blockData.seqNum)
  }

  private def notExpiredDeploy(earliestBlockNumber: Long, d: DeployData): Boolean =
    d.validAfterBlockNumber > earliestBlockNumber

  private def notFutureDeploy(currentBlockNumber: Long, d: DeployData): Boolean =
    d.validAfterBlockNumber < currentBlockNumber
}
