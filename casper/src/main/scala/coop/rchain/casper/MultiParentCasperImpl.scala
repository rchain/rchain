package coop.rchain.casper

import cats.data.{EitherT, OptionT}
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.BlockStatus.invalidMessageScope
import coop.rchain.casper.MultiParentCasperImpl._
import coop.rchain.casper.deploychainsetcasper.DeployChainSetCasper.{
  computeMessageScope,
  BlockMetadataCasper,
  BlockMetadataDag,
  BlockMetadataSafetyOracle
}
import coop.rchain.casper.deploychainsetcasper._
import coop.rchain.casper.merging.{BlockIndexer, DeployChainMerger}
import coop.rchain.casper.protocol._
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.v2.core.Casper._
import coop.rchain.casper.v2.stcasper.ConflictsResolver.ConflictResolution
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash._
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockHash => _, _}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared._
import coop.rchain.models.rholang.implicits._

import scala.collection.concurrent.TrieMap

// format: off
class MultiParentCasperImpl[F[_]
  /* Execution */   : Concurrent: Time
  /* Transport */   : EventPublisher
  /* Rholang */     : RuntimeManager
  /* Storage */     : BlockStore: BlockDagStorage: DeployStorage
  /* Diagnostics */ : Log: Metrics: Span] // format: on
(
    validatorId: Option[ValidatorIdentity],
    faultToleranceThreshold: Float,
    shardName: String
//    deployLifespan: Int,
//    casperVersion: Int,
//    faultToleranceThreshold: Float
) extends MultiParentCasper[F] {

  implicit private val logSource: LogSource = LogSource(this.getClass)

  // TODO: Extract hardcoded version from shard config
  private val version = 1L

  def getValidator: F[Option[ValidatorIdentity]] = validatorId.pure[F]

  def getVersion: F[Long] = version.pure[F]

  // TODO this does not make sense, now implemented to make other parts of the code happy
  override def lastFinalizedBlock: F[BlockMessage] =
    blockDag.map(
      dag =>
        BlockMessage(
          blockHash = ByteString.EMPTY,
          header = Header(
            timestamp = 0L,
            version = version
          ),
          body = Body(
            state = RChainState(
              preStateHash = ByteString.EMPTY,
              // only this matters
              postStateHash = dag.finalizationFringes.head.state,
              bonds = List(),
              blockNumber = -1
            ),
            deploys = List(),
            systemDeploys = List(),
            rejectedDeploys = List()
          ),
          justifications = List(),
          sender = ByteString.EMPTY,
          seqNum = -1,
          sig = ByteString.EMPTY,
          sigAlgorithm = "",
          shardId = ""
        )
    )

  def dagContains(hash: BlockHash): F[Boolean] = blockDag.flatMap(_.contains(hash))

  def deploy(d: Signed[DeployData]): F[Either[DeployError, DeployId]] =
    InterpreterUtil
      .mkTerm(d.data.term, NormalizerEnv(d))
      .bitraverse(
        err => DeployError.parsingError(s"Error in parsing term: \n$err").pure[F],
        _ => addDeploy(d)
      )

  def addDeploy(deploy: Signed[DeployData]): F[DeployId] =
    for {
      _ <- DeployStorage[F].add(List(deploy))
      _ <- Log[F].info(s"Received ${PrettyPrinter.buildString(deploy)}")
    } yield deploy.sig

  def blockDag: F[BlockDagRepresentation[F]] =
    BlockDagStorage[F].getRepresentation

  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    BlockDagStorage[F].accessEquivocationsTracker { tracker =>
      tracker.equivocationRecords.map { equivocations =>
        equivocations
          .map(_.equivocator)
          .flatMap(weights.get)
          .sum
          .toFloat / weightMapTotal(weights)
      }
    }

  def getRuntimeManager: F[RuntimeManager[F]] = Sync[F].delay(RuntimeManager[F])

  // Todo make this input pure BlockDagState
  override def getSnapshot(targetMessageOpt: Option[BlockMessage]): F[CasperSnapshot[F]] = {
    import cats.instances.list._
    import coop.rchain.blockstorage.syntax._

    def getLatestMessagesMetas(dag: BlockDagRepresentation[F]): F[Map[Validator, BlockMetadata]] =
      OptionT
        .fromOption[F](
          targetMessageOpt
            .map(_.justifications.map { case Justification(s, h) => (s, h) }.toMap)
        )
        .getOrElseF(dag.latestMessageHashes)
        .flatMap(_.toList.traverse { case (s, h) => dag.lookupUnsafe(h).map((s, _)) }.map(_.toMap))

    for {
      // the most recent view on the DAG, includes everything node seen so far
      dag <- BlockDagStorage[F].getRepresentation
      // justifications define the scope of the message (whether it is about to be created or validated)
      latestMessages <- getLatestMessagesMetas(dag)
      // find supermajority fringe
      r                                                 <- Stopwatch.duration(computeMessageScope(latestMessages.values.toSet, dag))
      (MessageScope(supermajorityFringe, _), timeScope) = r
      r <- Stopwatch.duration(
            if (dag.finalizationFringes.size <= 1)
              dag.finalizationFringes.head.pure[F]
            else
              dag.finalizationFringes
                .findM { fringe =>
                  for {
                    fringeMetas <- fringe.finalizationFringe
                                    .flatMap(_._2)
                                    .traverse(
                                      dag.lookupUnsafe(_)
                                    )
                    suitable = !supermajorityFringe
                      .flatMap(smm => fringeMetas.map((_, smm)))
                      .exists {
                        case (mCandidate, mSupermajority) =>
                          (mCandidate.sender == mSupermajority._1) && (mCandidate.seqNum > mSupermajority._2.head.seqNum)
                      }
                  } yield suitable
                }
                .flatMap { rOpt =>
                  rOpt.liftTo(
                    new Exception(
                      s"No fringe found in local database with supermajority against latest messages."
                    )
                  )
                }
          )
      (finalizationFringe, time) = r
      _ <- Log[F].info(
            s"Message scope computed in ${timeScope}. Finalization fringe found in $time (from ${dag.finalizationFringes.size})"
          )

      // some technical information
      maxBlockNum    = latestMessages.values.map(_.blockNum).max
      maxSeqNums     = latestMessages.values.map(m => (m.sender -> m.seqNum)).toMap
      invalidBlocks  <- dag.invalidBlocksMap
      deploysInScope = Set.empty[DeployId]
      // Todo read these from state
      deployLifespan = 50
      casperVersion  = 0L
    } yield CasperSnapshot(
      dag,
      finalizationFringe,
      latestMessages,
      invalidBlocks,
      deploysInScope,
      maxBlockNum,
      maxSeqNums,
      deployLifespan,
      shardName,
      casperVersion
    )
  }

  override def validate(
      b: BlockMessage,
      s: CasperSnapshot[F]
  ): F[Either[BlockError, ValidBlock]] = {
    val validationProcess: EitherT[F, BlockError, ValidBlock] =
      for {
        _ <- EitherT.fromEither(
              if (b.body.state.preStateHash == s.finalizedFringe.state)
                ValidBlock.asRight[BlockError]
              else invalidMessageScope.asLeft[ValidBlock]
            )
        _ <- EitherT(
              Validate
                .blockSummary(b, s, s.shardName, s.deployLifespan)
            )
        _ <- EitherT.liftF(Span[F].mark("post-validation-block-summary"))
        _ <- EitherT(
              InterpreterUtil
                .validateBlockCheckpoint(b, s, RuntimeManager[F])
                .map {
                  case Left(ex)       => Left(ex)
                  case Right(Some(_)) => Right(BlockStatus.valid)
                  case Right(None)    => Left(BlockStatus.invalidTransaction)
                }
            )
        _      <- EitherT.liftF(Span[F].mark("transactions-validated"))
        _      <- EitherT(Validate.bondsCache(b, RuntimeManager[F]))
        _      <- EitherT.liftF(Span[F].mark("bonds-cache-validated"))
        status <- EitherT(Validate.neglectedInvalidBlock(b, s))
//        _      <- EitherT.liftF(Span[F].mark("neglected-invalid-block-validated"))
//        lfb    <- EitherT.liftF(lastFinalizedBlock)
//        status <- EitherT(EquivocationDetector.checkNeglectedEquivocationsWithUpdate(b, s.dag, lfb))
//        _      <- EitherT.liftF(Span[F].mark("neglected-equivocation-validated"))
//        depDag <- EitherT.liftF(CasperBufferStorage[F].toDoublyLinkedDag)
//        status <- EitherT(EquivocationDetector.checkEquivocations(depDag, b, s.dag))
//        _      <- EitherT.liftF(Span[F].mark("equivocation-validated"))
      } yield status

    val validationProcessDiag = for {
      // Create block and measure duration
      r                    <- Stopwatch.duration(validationProcess.value)
      (valResult, elapsed) = r
      _ <- valResult
            .map { status =>
              val blockInfo   = PrettyPrinter.buildString(b, short = true)
              val deployCount = b.body.deploys.size
              Log[F].info(s"Block replayed: $blockInfo (${deployCount}d) ($status) [$elapsed]") <* {
                indexBlock(b) >> Log[F].info(s"Block indexed: $blockInfo")
              }
            }
            .getOrElse(().pure[F])
    } yield valResult

    Log[F].info(s"Validating block ${PrettyPrinter.buildString(b, short = true)}.") *> validationProcessDiag
  }

  override def handleValidBlock(
      b: BlockMessage,
      s: CasperSnapshot[F]
  ): F[BlockDagRepresentation[F]] = {

    def mergeF(
        state: StateHash,
        mergeScope: Set[(BlockMetadata, Set[BlockMetadata])]
    ): F[(ConflictResolution[DeployChain], StateHash)] = {

      val conflictResolver = for {
        genesis <- s.dag.genesis
        indicesMissing <- Sync[F].delay(
                           mergeScope
                             .flatMap(x => x._2 + x._1)
                             .map(_.blockHash)
                             .filterNot(_ == genesis)
                             .filter(!DeployChainMerger.blocksIndexed.keySet.contains(_))
                         )
        _ <- indicesMissing.toList.traverse(BlockStore[F].getUnsafe(_).flatMap(indexBlock[F]))
        r = DeployChainSetConflictResolver[F](x => DeployChainMerger.indexCache(x).pure[F])
      } yield r

      for {
        _        <- Log[F].info(s"Merging FF")
        resolver <- conflictResolver
        genesis  <- s.dag.genesis
        clearedMergeScope <- mergeScope.toMap
                            // hack as no mergeable chans for genesis
                              .mapValues(_.filterNot(_.blockHash == genesis))
                              .toList
                              .traverse {
                                case (c, f) =>
                                  resolver
                                    .clean(
                                      DeployChainMerger.blocksIndexed(c.blockHash),
                                      f.flatMap(x => DeployChainMerger.blocksIndexed(x.blockHash))
                                    )
                                    .map((c, _))
                              }
        resolution <- resolver.resolve(
                       clearedMergeScope
                         .flatMap(
                           v => DeployChainMerger.blocksIndexed(v._1.blockHash)
                         )
                         .toSet,
                       ConflictResolution(Set(), clearedMergeScope.flatMap(_._2).toSet)
                     )

        r <- Stopwatch.duration(
              DeployChainMerger.merge(
                Blake2b256Hash.fromByteString(state),
                resolution.acceptedSet
              )(RuntimeManager[F])
            )
        (
          (
            finalizedState,
            actionsNum,
            trieActionComputeTime,
            stateComputedTime
          ),
          mergeTime
        ) = r
        _ <- Log[F].info(
              s"Finalization fringe merged in $mergeTime: ${resolution.acceptedSet.size} DC merged " +
                s"${actionsNum} trie actions computed in $trieActionComputeTime, applied in $stateComputedTime. "
            )
      } yield (resolution, finalizedState.toByteString)
    }

    for {
      // Todo index only valid block
      //_ <- indexBlock(b)
      _ <- Log[F].info(s"Inserting block ${b.blockHash.show}")
      r <- BlockDagStorage[F].insert(b, invalid = false, (mergeF _).some)
    } yield r
  }

  override def handleInvalidBlock(
      block: BlockMessage,
      status: InvalidBlock,
      s: CasperSnapshot[F]
  ): F[BlockDagRepresentation[F]] = {
    // TODO: Slash block for status except InvalidUnslashableBlock
    def handleInvalidBlockEffect(
        status: BlockError,
        block: BlockMessage
    ): F[BlockDagRepresentation[F]] =
      for {
        _ <- Log[F].warn(
              s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for ${status.toString}."
            )

        r <- BlockDagStorage[F].insert(block, invalid = true)
      } yield r

    status match {
      case InvalidBlock.AdmissibleEquivocation =>
        val baseEquivocationBlockSeqNum = block.seqNum - 1
        for {
          _ <- BlockDagStorage[F].accessEquivocationsTracker { tracker =>
                for {
                  equivocations <- tracker.equivocationRecords
                  _ <- Sync[F].unlessA(equivocations.exists {
                        case EquivocationRecord(validator, seqNum, _) =>
                          block.sender == validator && baseEquivocationBlockSeqNum == seqNum
                        // More than 2 equivocating children from base equivocation block and base block has already been recorded
                      }) {
                        val newEquivocationRecord =
                          EquivocationRecord(
                            block.sender,
                            baseEquivocationBlockSeqNum,
                            Set.empty[BlockHash]
                          )
                        tracker.insertEquivocationRecord(newEquivocationRecord)
                      }
                } yield ()
              }
          // We can only treat admissible equivocations as invalid blocks if
          // casper is single threaded.
          updatedDag <- handleInvalidBlockEffect(InvalidBlock.AdmissibleEquivocation, block)
        } yield updatedDag

      case InvalidBlock.IgnorableEquivocation =>
        /*
         * We don't have to include these blocks to the equivocation tracker because if any validator
         * will build off this side of the equivocation, we will get another attempt to add this block
         * through the admissible equivocations.
         */
        Log[F]
          .info(
            s"Did not add block ${PrettyPrinter.buildString(block.blockHash)} as that would add an equivocation to the BlockDAG"
          )
          .as(s.dag)

      case ib: InvalidBlock if InvalidBlock.isSlashable(ib) =>
        handleInvalidBlockEffect(ib, block)

      case ib: InvalidBlock =>
        Log[F]
          .warn(
            s"Recording invalid block ${PrettyPrinter.buildString(block.blockHash)} for $ib."
          )
          .as(s.dag)
    }
  }
}

object MultiParentCasperImpl {

  // TODO: Extract hardcoded deployLifespan from shard config
  // Size of deploy safety range.
  // Validators will try to put deploy in a block only for next `deployLifespan` blocks.
  // Required to enable protection from re-submitting duplicate deploys
  val deployLifespan = 50

  def addedEvent(block: BlockMessage): RChainEvent = {
    val (blockHash, justifications, deployIds, creator, seqNum) = blockEvent(block)
    RChainEvent.blockAdded(
      blockHash,
      justifications,
      deployIds,
      creator,
      seqNum
    )
  }

  def createdEvent(b: BlockMessage): RChainEvent = {
    val (blockHash, justifications, deployIds, creator, seqNum) = blockEvent(b)
    RChainEvent.blockCreated(
      blockHash,
      justifications,
      deployIds,
      creator,
      seqNum
    )
  }

  private def blockEvent(block: BlockMessage) = {

    val blockHash = block.blockHash.base16String
    val justificationHashes =
      block.justifications.toList
        .map(j => (j.validator.base16String, j.latestBlockHash.base16String))
    val deployIds: List[String] =
      block.body.deploys.map(pd => PrettyPrinter.buildStringNoLimit(pd.deploy.sig))
    val creator = block.sender.base16String
    val seqNum  = block.seqNum
    (blockHash, justificationHashes, deployIds, creator, seqNum)
  }

  def indexBlock[F[_]: Concurrent: RuntimeManager](b: BlockMessage): F[Unit] = {
    val blockPreState  = b.body.state.preStateHash
    val blockPostState = b.body.state.postStateHash
    val blockSender    = b.sender.toByteArray
    for {
      mergeableChs <- RuntimeManager[F].loadMergeableChannels(
                       blockPostState,
                       if (b.seqNum == 0) Array() else blockSender,
                       b.seqNum
                     )
      index <- BlockIndexer(
                b.blockHash,
                b.body.deploys,
                b.body.systemDeploys,
                blockPreState.toBlake2b256Hash,
                blockPostState.toBlake2b256Hash,
                RuntimeManager[F].getHistoryRepo,
                mergeableChs
              )
      _ = index.map { case (k, v) => DeployChainMerger.indexCache.putIfAbsent(k, v) }
      _ = DeployChainMerger.blocksIndexed.putIfAbsent(b.blockHash, index.keySet)
    } yield ()
  }

//  def computeScope[F[_]: Concurrent: RuntimeManager: BlockStore: Log: Metrics: BlockDagStorage](
//      latestMessages: Set[BlockMetadata],
//      dag: BlockDagRepresentation[F]
//  ): F[(MessageScope[BlockMetadata], String, Blake2b256Hash)] =
//    for {
//      // scope of the message
//      r                             <- Stopwatch.duration(computeMessageScope(latestMessages, dag))
//      (messageScope, messageScopeT) = r
//
//      indicesMissing = messageScope.conflictScope.v
//        .map(_.blockHash)
//        .filter(!DeployChainMerger.blocksIndexed.keySet.contains(_))
//      _ <- indicesMissing.toList.traverse(BlockStore[F].getUnsafe(_).flatMap(indexBlock[F]))
//
//      mergeFringe = mergeFinalizationFringe(
//        messageScope.finalizationFringe.v,
//        dag,
//        finalizationState.rejected,
//        hashes =>
//          hashes.traverse { h =>
//            BlockStore[F]
//              .getUnsafe(h)
//              .flatMap(indexBlock[F])
//              .unlessA(DeployChainMerger.blocksIndexed.contains(h))
//          }.void
//      )(RuntimeManager[F])
//      finalizedState <- OptionT
//                         .fromOption[F](finalizedStateCache.get(messageScope.finalizationFringe.v))
//                         .getOrElseF(
//                           mergeFringe.map { v =>
//                             val _ = MultiParentCasperImpl.finalizedStateCache
//                               .update(messageScope.finalizationFringe.v, v)
//                             v
//                           }
//                         )
//    } yield (messageScope, messageScopeT, finalizedState)
}
