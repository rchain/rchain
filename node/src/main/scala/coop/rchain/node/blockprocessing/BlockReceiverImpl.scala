package coop.rchain.node.blockprocessing

import cats.data.EitherT
import cats.effect.Concurrent
import cats.effect.concurrent.{Ref, Semaphore}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.FlatCasperBufferKeyValueStorage.CasperBufferStorage
import coop.rchain.blockstorage.dag.state.BlockDagBufferState.MessageStatus
import coop.rchain.blockstorage.dag.state.BlockDagState
import coop.rchain.blockstorage.dag.state.BlockDagState.AckReceivedResult
import coop.rchain.casper.{PrettyPrinter, Validate}
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.{Log, Time}
import coop.rchain.casper.processing.MessageReceiver
import coop.rchain.casper.processing.MessageReceiver._
import fs2.Stream

import scala.collection.concurrent.TrieMap

final case class BlockReceiverImpl[F[_]: Concurrent: Log: Span: Metrics: Time: BlockStore](
    override val input: Stream[F, BlockHash],
    block: TrieMap[BlockHash, BlockMessage], // this serves as a buffer to address full message
    blockDagState: Ref[F, BlockDagState],
    bufferStorage: CasperBufferStorage[F],
    blockDagUpdateLock: Semaphore[F]
) extends MessageReceiver[F, BlockHash, BlockDagState] {

  override def checkIgnore(
      m: BlockHash
  ): EitherT[F, MessageReceiver.ReceiveReject, BlockHash] = {
    val signatureIsValidF = Validate.blockSignature(block(m))
    for {
      dagState <- EitherT.liftF(blockDagState.get)
      _        <- EitherT.fromOption((!dagState.validated(m)).guard[Option].as(m), validated)
      _ <- EitherT.fromOption(
            (!dagState.pendingValidation(m)).guard[Option].as(m),
            pendingValidation
          )
      _ <- EitherT.fromOption(
            (!dagState.validationInProgress(m)).guard[Option].as(m),
            validationInProgress
          )
      r <- EitherT.fromOptionF(signatureIsValidF.map(_.guard[Option].as(m)), signatureInvalid)
    } yield r
  }

  import coop.rchain.models.syntax._
  override def diagRejected(m: BlockHash, r: MessageReceiver.ReceiveReject): F[Unit] =
    Log[F].debug(s"Message ${m.show.take(10)} rejected :$r")

  override def store(message: BlockHash): F[Unit] = BlockStore[F].put(block(message))

  override def receivedEffect(
      message: BlockHash
  ): F[ReceiveResult[BlockHash, BlockDagState]] =
    blockDagState
      .modify { state =>
        if (state.received(message))
          (
            state,
            (
              Map.empty[BlockHash, MessageStatus],
              ReceiveResult(state, Set.empty[BlockHash], Set.empty[BlockHash])
            )
          )
        else {
          val AckReceivedResult(newState, changes, dependenciesPending, dependenciesToRequest) =
            state.ackReceived(
              message,
              block(message).justifications.map(_.latestBlockHash).toSet
            )
          (
            newState,
            (changes, ReceiveResult(newState, dependenciesPending, dependenciesToRequest))
          )
        }
      }
      .flatMap {
        case (changes, r) =>
          // remove block only when receive is ack-ed. This is to not remove twice in case of concurrent receive effects.
          (bufferStorage.put(changes.toSeq) >> block.remove(message).pure)
            .whenA(changes.nonEmpty)
            .as(r)
      }
      // clean blocks map buffer to not leak memory
      .flatTap { r =>
        Log[F].info(
          s"receivedEffect toReq:${PrettyPrinter
            .buildString(r.dependenciesToRetrieve)} pending: ${PrettyPrinter
            .buildString(r.dependenciesPending)}"
        )
      }
}

object BlockReceiverImpl {
  type ReceiveBlock[F[_]] = BlockMessage => F[Unit]

  def apply[F[_]: Concurrent: Log: Span: Metrics: Time](
      input: Stream[F, BlockMessage],
      blockDagState: Ref[F, BlockDagState],
      bufferStorage: CasperBufferStorage[F],
      blockStore: BlockStore[F],
      blockDagUpdateLock: Semaphore[F]
  ): MessageReceiver[F, BlockHash, BlockDagState] = {
    // This is make implementation meet trait, to work on BlockHash not full message
    val buffer      = TrieMap.empty[BlockHash, BlockMessage]
    implicit val bs = blockStore

    val s = input
      .map { b =>
        buffer.update(b.blockHash, b)
        b.blockHash
      }
    BlockReceiverImpl[F](s, buffer, blockDagState, bufferStorage, blockDagUpdateLock)
  }
}
