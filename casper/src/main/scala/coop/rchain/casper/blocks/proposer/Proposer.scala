package coop.rchain.casper.blocks.proposer

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref, Semaphore}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{Casper, _}
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.Signed
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.{EventPublisher, Log, Stopwatch, Time}
import fs2.Stream

sealed abstract class ProposerResult
object ProposerEmpty                                                         extends ProposerResult
final case class ProposerSuccess(status: ProposeStatus, block: BlockMessage) extends ProposerResult
final case class ProposerFailure(status: ProposeStatus, seqNumber: Long)     extends ProposerResult
final case class ProposerStarted(seqNumber: Long)                            extends ProposerResult

object ProposerResult {
  def empty: ProposerResult = ProposerEmpty
  def success(status: ProposeStatus, block: BlockMessage): ProposerResult =
    ProposerSuccess(status, block)
  def failure(status: ProposeStatus, seqNumber: Long): ProposerResult =
    ProposerFailure(status, seqNumber)
  def started(seqNumber: Long): ProposerResult = ProposerStarted(seqNumber)
}

class Proposer[F[_]: Concurrent: Log: Span](
    // base state on top of which block will be created
    getCasperSnapshot: F[CasperSnapshot[F]],
    createBlock: (
        CasperSnapshot[F],
        ValidatorIdentity
    ) => F[BlockCreatorResult],
    validateBlock: (CasperSnapshot[F], BlockMessage) => F[ValidBlockProcessing],
    proposeEffect: (BlockMessage, CasperSnapshot[F]) => F[Unit],
    validator: ValidatorIdentity,
    loadDeploys: F[Set[Signed[DeployData]]]
) {
  import coop.rchain.models.syntax._

  private def checkSyncConstr(s: CasperSnapshot[F]): F[Boolean] = {
    val selfJ = s.latestMessages
      .find {
        case (v, _) => v.toByteArray sameElements validator.publicKey.bytes
      }
      .map(_._2)

    val prevJs  = selfJ.map(_.justifications.map(_.latestBlockHash)).getOrElse(List())
    val newMsgs = s.latestMessages.values.map(_.blockHash).toSet -- prevJs
    //    -- selfJ
    //      .map(m => Set(m.blockHash))
    //      .getOrElse(Set())
    val syncV = (newMsgs.size.toFloat / prevJs.size)
    (syncV > 0.67).pure[F]

//    for {
//      fms <- s.finalizedFringe.finalizationFringe.toList.traverse {
//              case (v, h) =>
//                h.toList.traverse(s.dag.lookupUnsafe).map(_.map(_.seqNum).max).map((v, _))
//            }
//      lms       = s.latestMessages.map { case (v, m) => (v, m.seqNum) }
//      distances = fms.map { case (v, h) => (v, lms(v) - h) }.sortBy { case (_, d) => d }.reverse
//      // if you are in 1/2 of fastest - you are rabbit
//      rabbits = distances.take(distances.size / 2)
//      turtles = distances.drop(distances.size / 2)
//      slowDown = rabbits
//        .find(_._1.toByteArray sameElements validator.publicKey.bytes)
//        .exists { case (_, v) => v > turtles.head._2 }
//      _ <- Log[F].info(s"unfin distances: ${distances.map(_._2)}")
//
//    } yield true //!slowDown
  }

//  private def acquiescence(s: CasperSnapshot[F]) = {
//    val parentStates = s.latestMessages.map(_._2.postStateHash).toSet
//    loadDeploys.map(
//      _.isEmpty && parentStates.size == 1 && s.finalizedFringe.state == parentStates.head
//    )
//  }

  implicit val RuntimeMetricsSource: Source = Metrics.Source(CasperMetricsSource, "proposer")
  // This is the whole logic of propose
  private def doPropose(
      s: CasperSnapshot[F]
  ): F[(ProposeResult, Option[BlockMessage])] =
    Span[F].traceI("do-propose") {
      for {
        syncOK <- checkSyncConstr(s)
        acq    <- false.pure[F] //acquiescence(s)
        b      <- if (syncOK && !acq) createBlock(s, validator) else NotEnoughNewBlocks.pure[F]
        r <- b match {
              case Created(b) =>
                validateBlock(s, b).flatMap {
                  case Right(v) =>
                    proposeEffect(b, s) >>
                      (ProposeResult.success(v), b.some).pure[F]
                  case Left(v) =>
                    Concurrent[F].raiseError[(ProposeResult, Option[BlockMessage])](
                      new Throwable(
                        s"Validation of self created block failed with reason: $v, cancelling propose."
                      )
                    )
                }
              case NotEnoughNewBlocks =>
                Log[F]
                  .info(s"Not enough blocks. Canceling propose")
                  .as((ProposeResult.notEnoughBlocks, none[BlockMessage]))
            }
      } yield r
    }

  def propose(
      isAsync: Boolean,
      proposeIdDef: Deferred[F, ProposerResult]
  ): F[(ProposeResult, Option[BlockMessage])] = {
    def getValidatorNextSeqNumber(cs: CasperSnapshot[F]): Long = {
      val valBytes = ByteString.copyFrom(validator.publicKey.bytes)
      cs.maxSeqNums.getOrElse(valBytes, 0L) + 1
    }
    for {
      // get snapshot to serve as a base for propose
      s <- Stopwatch.time(Log[F].info(_))(s"getCasperSnapshot")(getCasperSnapshot)
      result <- if (isAsync) for {
                 nextSeq <- getValidatorNextSeqNumber(s).pure[F]
                 _       <- proposeIdDef.complete(ProposerResult.started(nextSeq))
                 // propose
                 r <- doPropose(s)
               } yield r
               else
                 for {
                   // propose
                   r                      <- doPropose(s)
                   (result, blockHashOpt) = r
                   proposerResult = blockHashOpt.fold {
                     val seqNumber = getValidatorNextSeqNumber(s)
                     ProposerResult.failure(result.proposeStatus, seqNumber)
                   } { block =>
                     ProposerResult.success(result.proposeStatus, block)
                   }
                   _ <- proposeIdDef.complete(proposerResult)
                 } yield r

    } yield result
  }
}

object Proposer {
  // format: off
  def apply[F[_]
    /* Execution */   : Concurrent: Time
    /* Storage */     : BlockStore: BlockDagStorage: DeployStorage
    /* Diagnostics */ : Log: Span: Metrics: EventPublisher
    /* Comm */        : CommUtil: BlockRetriever
  ] // format: on
  (
      validatorIdentity: ValidatorIdentity,
      dummyDeployOpt: Option[(PrivateKey, String)] = None,
      casperConf: CasperConf,
      blockDagStateRef: Ref[F, BlockDagState],
      blockDagUpdateLock: Semaphore[F]
  )(implicit runtimeManager: RuntimeManager[F]): Proposer[F] = {
    val getCasperSnapshot = new MultiParentCasperImpl(
      validatorIdentity.some,
      casperConf.faultToleranceThreshold,
      casperConf.shardName,
      casperConf.minPhloPrice
    ).getSnapshot()

    val createBlock = (s: CasperSnapshot[F], validatorIdentity: ValidatorIdentity) =>
      BlockCreator.create(s, validatorIdentity, dummyDeployOpt)

    val validateBlock = (s: CasperSnapshot[F], b: BlockMessage) =>
      new MultiParentCasperImpl(
        validatorIdentity.some,
        casperConf.faultToleranceThreshold,
        casperConf.shardName,
        casperConf.minPhloPrice
      ).validate(b, s)

    val proposeEffect = (b: BlockMessage, s: CasperSnapshot[F]) =>
      // store block
      BlockStore[F].put(b) >>
        blockDagUpdateLock
          .withPermit(
            for {
              dag <- new MultiParentCasperImpl(
                      validatorIdentity.some,
                      casperConf.faultToleranceThreshold,
                      casperConf.shardName,
                      casperConf.minPhloPrice
                    ).handleValidBlock(b, s)
              _ <- blockDagStateRef.update(_.ackValidated(b.blockHash, dag.getPureState).newState)
              _ <- BlockRetriever[F].ackInCasper(b.blockHash)
            } yield dag
            // inform block retriever about block
          ) >>
        // broadcast hash to peers
        CommUtil[F].sendBlockHash(b.blockHash, b.sender) >>
        // Publish event
        EventPublisher[F].publish(MultiParentCasperImpl.createdEvent(b))

    val loadDeploys = DeployStorage[F].readAll

    new Proposer(
      getCasperSnapshot,
      createBlock,
      validateBlock,
      proposeEffect,
      validatorIdentity,
      loadDeploys
    )
  }
}
