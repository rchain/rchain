package coop.rchain.node.blockprocessing

import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.CasperBufferKeyValueStorage.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagBufferState.ValidationInProgress
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.dag.state.BlockDagState.ValidatedResult
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.v2.core.Validation.Offence
import coop.rchain.casper.v2.processing.MessageValidator
import coop.rchain.casper.v2.processing.MessageValidator.ValidationResult
import coop.rchain.casper.{
  BlockStatus,
  CasperConf,
  InvalidBlock,
  MultiParentCasperImpl,
  NetworkSnapshot,
  PrettyPrinter,
  ValidatorIdentity
}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax._
import coop.rchain.shared.{EventPublisher, Log, Time}
import coop.rchain.shared.syntax._
import fs2.concurrent.Queue
import fs2.Stream

// format: off
final case class BlockValidatorImpl[F[_] 
/* Execution */   : Concurrent: Time
/* Transport */   : EventPublisher
/* Rholang */     : RuntimeManager
/* Storage */     : BlockStore: BlockDagStorage: DeployStorage
/* Diagnostics */ : Log: Metrics: Span] // format: on
(
    override val input: Stream[F, BlockHash],
    append: BlockHash => F[Unit],
    blockDagStateRef: Ref[F, BlockDagState],
    casperConf: CasperConf,
    bufferStorage: CasperBufferStorage[F]
) extends MessageValidator[F, BlockHash, BlockDagState] {

  override def validate(message: BlockHash): F[ValidationResult[BlockHash, BlockDagState]] = {
    // Todo get rid of MultiParentCasperImpl completely, use DeployChainSetCaper
    val casper = new MultiParentCasperImpl[F](
      validatorId = none[ValidatorIdentity], // this does not matter
      casperConf.faultToleranceThreshold,
      casperConf.shardName
    )
    for {
      _                <- Log[F].info(s"Validating ${message.show}.")
      block            <- BlockStore[F].getUnsafe(message)
      snapshot         <- casper.getSnapshot(block.some) // Todo create snapshot from latest in blockDagStatRef
      validationResult <- casper.validate(block, snapshot)
      offenceOpt       = validationResult.swap.toOption.map(BlockStatus.toOffence)
      _                <- Log[F].info(s"Validating ${message.show}. Done. Invoking Effects.")
      // Todo implement precise offence storage + make pure state output changes for storage, not vice versa
      newRepr <- validationResult match {
                  case Left(v: InvalidBlock) => casper.handleInvalidBlock(block, v, snapshot)
                  case _                     => casper.handleValidBlock(block, snapshot)
                }
      _ <- offenceOpt.traverse(o => Log[F].info(s"Validating ${message.show}. Done. Invalid: ${o}"))
      r <- blockDagStateRef.modify { st =>
            val ValidatedResult(newSt, unlockedChildren) =
              st.ackValidated(message, newRepr.getPureState)
            (newSt, ValidationResult(newSt, unlockedChildren))
          }
      _ <- bufferStorage.delete(message)
      _ <- Log[F].info(s"Validating ${message.show}. Done. Invoking Effects. Done.")
    } yield r
  }
  override def appendToInput(message: BlockHash): F[Unit] = append(message)
}
object BlockValidatorImpl {
  type ValidateBlock[F[_]] = BlockHash => F[Unit]

  def apply[F[_]: Concurrent: Time: EventPublisher: Log: Metrics: Span](
      blockDagStateRef: Ref[F, BlockDagState],
      casperShardConf: CasperConf,
      blockStore: BlockStore[F],
      dagStore: BlockDagStorage[F],
      bufferStore: CasperBufferStorage[F],
      deployStore: DeployStorage[F],
      runtimeManager: RuntimeManager[F]
  ): F[MessageValidator[F, BlockHash, BlockDagState]] = {
    implicit val (bs, bds, rm, ds) =
      (blockStore, dagStore, runtimeManager, deployStore)
    for {
      // Adjust this to modify behaviour of validation
      validationQueue <- Queue.unbounded[F, BlockHash]
      stream          = validationQueue.dequeueChunk(1)
      append          = validationQueue.enqueue1 _
      // Send to validation blocks that are in pending validation status
      // (e.g. node has been restarted in the middle of validation)
      // Note: validation will be started when stream is started
      dagState          <- blockDagStateRef.get
      pendingValidation = dagState.buffer.collect { case (h, ValidationInProgress) => h }
      _                 <- pendingValidation.toList.traverse(append)
      _ <- Log[F].info(
            s"Pending blocks added to validation queue: ${PrettyPrinter.buildString(pendingValidation.toList)}."
          )
    } yield BlockValidatorImpl(
      stream,
      append,
      blockDagStateRef,
      casperShardConf,
      bufferStore
    )
  }

}
