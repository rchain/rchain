package coop.rchain.casper.blocks.proposer

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref, Semaphore}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.casper.MultiParentCasperImpl.computeScope
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
final case class ProposerFailure(status: ProposeStatus, seqNumber: Int)      extends ProposerResult
final case class ProposerStarted(seqNumber: Int)                             extends ProposerResult

object ProposerResult {
  def empty: ProposerResult = ProposerEmpty
  def success(status: ProposeStatus, block: BlockMessage): ProposerResult =
    ProposerSuccess(status, block)
  def failure(status: ProposeStatus, seqNumber: Int): ProposerResult =
    ProposerFailure(status, seqNumber)
  def started(seqNumber: Int): ProposerResult = ProposerStarted(seqNumber)
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
    validator: ValidatorIdentity
) {

  implicit val RuntimeMetricsSource: Source = Metrics.Source(CasperMetricsSource, "proposer")
  // This is the whole logic of propose
  private def doPropose(
      s: CasperSnapshot[F]
  ): F[(ProposeResult, Option[BlockMessage])] =
    Span[F].traceI("do-propose") {
      for {
        b <- createBlock(s, validator)
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
            }
      } yield r
    }

  def propose(
      isAsync: Boolean,
      proposeIdDef: Deferred[F, ProposerResult]
  ): F[(ProposeResult, Option[BlockMessage])] = {
    def getValidatorNextSeqNumber(cs: CasperSnapshot[F]): Int = {
      val valBytes = ByteString.copyFrom(validator.publicKey.bytes)
      cs.maxSeqNums.getOrElse(valBytes, 0) + 1
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
      casperConf.shardName
    ).getSnapshot()

    val createBlock = (s: CasperSnapshot[F], validatorIdentity: ValidatorIdentity) =>
      BlockCreator.create(s, validatorIdentity, dummyDeployOpt)

    val validateBlock = (s: CasperSnapshot[F], b: BlockMessage) =>
      new MultiParentCasperImpl(
        validatorIdentity.some,
        casperConf.faultToleranceThreshold,
        casperConf.shardName
      ).validate(b, s)

    val proposeEffect = (b: BlockMessage, s: CasperSnapshot[F]) =>
      // store block
      BlockStore[F].put(b) >>
        blockDagUpdateLock.withPermit(
          // save changes to Casper
          new MultiParentCasperImpl(
            validatorIdentity.some,
            casperConf.faultToleranceThreshold,
            casperConf.shardName
          ).handleValidBlock(b, s, false).flatMap { dag =>
            blockDagStateRef.update(_.ackValidated(b.blockHash, dag.getPureState).newState) >>
              MultiParentCasperImpl.updateLatestScope(dag)
          } >>
            // inform block retriever about block
            BlockRetriever[F].ackInCasper(b.blockHash)
        ) >>
        // broadcast hash to peers
        CommUtil[F].sendBlockHash(b.blockHash, b.sender) >>
        // Publish event
        EventPublisher[F].publish(MultiParentCasperImpl.createdEvent(b))

//    val loadDeploys = DeployStorage[F].readAll

    new Proposer(
      getCasperSnapshot,
      createBlock,
      validateBlock,
      proposeEffect,
      validatorIdentity
    )
  }
}
