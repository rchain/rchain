package coop.rchain.node.blockprocessing

import cats.effect.Concurrent
import cats.effect.concurrent.{Ref, Semaphore}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.FlatCasperBufferKeyValueStorage.CasperBufferStorage
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.state.BlockDagBufferState.ValidationInProgress
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.dag.state.BlockDagState.ValidatedResult
import coop.rchain.blockstorage.deploy.DeployStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.processing.MessageValidator
import coop.rchain.casper.processing.MessageValidator.ValidationResult
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{BlockStatus, _}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax._
import coop.rchain.shared.syntax._
import coop.rchain.shared.{EventPublisher, Log, Time}
import fs2.Stream
import fs2.concurrent.Queue

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
    blockDagStatRef: Ref[F, BlockDagState],
    casperConf: CasperConf,
    bufferStorage: CasperBufferStorage[F],
    blockDagUpdateLock: Semaphore[F]
) extends MessageValidator[F, BlockHash, BlockDagState] {

  override def validate(message: BlockHash): F[ValidationResult[BlockHash, BlockDagState]] = {
    // Todo get rid of MultiParentCasperImpl completely, use DeployChainSetCaper
    val casper = new MultiParentCasperImpl[F](
      validatorId = none[ValidatorIdentity], // this does not matter
      casperConf.faultToleranceThreshold,
      casperConf.shardName,
      casperConf.minPhloPrice
    )
    for {
      _                <- Log[F].info(s"Validating ${message.show.take(10)}.")
      block            <- BlockStore[F].getUnsafe(message)
      snapshot         <- casper.getSnapshot(block.some) // Todo create snapshot from latest in blockDagStatRef
      validationResult <- casper.validate(block, snapshot)
      _                <- Log[F].info(s"Validating ${message.show.take(10)}. Done. Invoking Effects.")
      r <- blockDagUpdateLock.withPermit(
            for {
              // Todo implement precise offence storage + make pure state output changes for storage, not vice versa
              newRepr <- validationResult match {
                          case Left(v: InvalidBlock) =>
                            Log[F].info(
                              s"Validating ${message.show.take(10)}. Done. Block invalid."
                            ) >> casper.handleInvalidBlock(block, v, snapshot)

                          case _ => casper.handleValidBlock(block, snapshot)
                        }
              // Todo This should be in the same lock that BlockDagStorage.insert is using
              r <- blockDagStatRef.modify { st =>
                    val ValidatedResult(newSt, unlockedChildren) =
                      st.ackValidated(message, newRepr.getPureState)
                    (newSt, ValidationResult(newSt, unlockedChildren))
                  }
              _ <- bufferStorage.put(r.dependentUnlocked.map(_ -> ValidationInProgress).toList)
              _ <- bufferStorage.delete(message)
            } yield r
          )
      _ <- Log[F].info(
            s"Validating ${message.show.take(100)}. Done. Invoking Effects. Done. Unlocked: [${r.dependentUnlocked
              .map(_.show.take(10))
              .mkString("; ")}]."
          )
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
      runtimeManager: RuntimeManager[F],
      blockDagUpdateLock: Semaphore[F]
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
      bufferStore,
      blockDagUpdateLock
    )
  }

}
