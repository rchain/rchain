package coop.rchain.casper.util.comm

import scala.collection.immutable.Queue

import cats.{Eq, Show}
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._

import FairRoundRobinDispatcher._
import coop.rchain.shared.Log

class FairRoundRobinDispatcher[F[_]: Sync: Log, S: Show, M: Show: Eq](
    filter: M => F[Dispatch],
    handle: (S, M) => F[Unit],
    queue: Ref[F, Queue[S]],
    messages: Ref[F, Map[S, Queue[M]]],
    retries: Ref[F, Map[S, Int]],
    skipped: Ref[F, Int],
    maxSourceQueueSize: Int,
    giveUpAfterSkipped: Int,
    dropSourceAfterRetries: Int
) {
  assert(maxSourceQueueSize > 0)
  assert(giveUpAfterSkipped >= 0)
  assert(dropSourceAfterRetries >= 0)

  def dispatch(source: S, message: M): F[Unit] =
    filter(message).flatMap {
      case Handle =>
        ensureSourceExists(source) >>
          isDuplicate(source, message).ifM(
            Log[F].info(s"Dropped duplicate message ${message.show} from ${source.show}"),
            enqueueMessage(source, message) >> handleMessages
          )
      case Pass => handle(source, message)
      case Drop => ().pure
    }

  private[comm] def ensureSourceExists(source: S): F[Unit] =
    messages.get
      .map(_.contains(source))
      .ifM(
        ().pure,
        queue.update(source +: _) *>
          messages.update(_ + (source -> Queue.empty[M])) *>
          retries.update(_ + (source  -> 0)) *>
          Log[F].info(s"Added ${source.show} to the dispatch queue")
      )

  private[comm] def isDuplicate(source: S, message: M): F[Boolean] =
    messages.get.map(_(source).exists(_ === message))

  private[comm] def enqueueMessage(source: S, message: M): F[Unit] =
    messages.get
      .map(_(source).size < maxSourceQueueSize)
      .ifM(
        messages
          .modify { ps =>
            val q = ps(source).enqueue(message)
            (ps.updated(source, q), q)
          }
          .flatMap { q =>
            Log[F].info(
              s"Enqueued message ${message.show} from ${source.show} (queue length: ${q.length})"
            )
          } *>
          retries.update(_.updated(source, 0)),
        Log[F].info(s"Dropped message ${message.show} from ${source.show}")
      )

  private[comm] val handleMessages: F[Unit] = {
    def onSuccess: S => F[Unit] = success(_) >> handleNextMessage(onSuccess, _ => ().pure)
    def onFailure: S => F[Unit] = failure(_).ifM(handleMessages, ().pure)
    handleNextMessage(onSuccess, onFailure)
  }

  private[comm] def handleNextMessage(onSuccess: S => F[Unit], onFailure: S => F[Unit]): F[Unit] =
    queue.get.map(_.head).flatMap(s => handleMessage(s).ifM(onSuccess(s), onFailure(s)))

  private[comm] def handleMessage(source: S): F[Boolean] =
    messages.get.map(_(source).headOption) >>=
      (_.fold(false.pure)(handle(source, _).attempt.as(true)))

  private[comm] val rotate: F[Unit] =
    queue.modify(
      _.dequeue match {
        case (s, q) => (q.enqueue(s), q.headOption.getOrElse(s))
      }
    ) >>= (s => Log[F].info(s"It's ${s.show} turn"))

  private[comm] def dropSource(source: S): F[Unit] =
    queue.update(_.filterNot(_ == source)) *>
      messages.update(_ - source) *>
      retries.update(_ - source) *>
      Log[F].info(s"Dropped ${source.show} from the dispatch queue") *>
      queue.get >>= (q => Log[F].info(s"It's ${q.head.show} turn"))

  private[comm] def giveUp(source: S): F[Unit] =
    Log[F].info(s"Giving up on ${source.show}") *>
      skipped.update(_ => 0) *>
      retries.update(r => r.updated(source, r(source) + 1)) >>
      retries.get.map(_(source) > dropSourceAfterRetries).ifM(dropSource(source), rotate)

  private[comm] def success(source: S): F[Unit] =
    messages
      .modify { ms =>
        val (m, q) = ms(source).dequeue
        (ms.updated(source, q), m)
      }
      .flatMap(m => Log[F].info(s"Dispatched message ${m.show} from ${source.show}")) *>
      skipped.update(_ => 0) *>
      rotate

  // true if gave up
  private[comm] def failure(source: S): F[Boolean] =
    Log[F].info(s"No message to dispatch for ${source.show}") *>
      skipped.update(_ + 1) >>
      skipped.get.map(_ < giveUpAfterSkipped).ifM(false.pure, giveUp(source).as(true))
}

object FairRoundRobinDispatcher {

  sealed trait Dispatch
  object Handle extends Dispatch
  object Pass   extends Dispatch
  object Drop   extends Dispatch

  object Dispatch {
    val handle: Dispatch = Handle
    val pass: Dispatch   = Pass
    val drop: Dispatch   = Drop
  }

  def apply[F[_]: Sync: Log, S: Show, M: Show: Eq](
      filter: M => F[Dispatch],
      handle: (S, M) => F[Unit],
      maxSourceQueueSize: Int,
      giveUpAfterSkipped: Int,
      dropSourceAfterRetries: Int
  ): F[FairRoundRobinDispatcher[F, S, M]] =
    for {
      queue   <- Ref.of(Queue.empty[S])
      packets <- Ref.of(Map.empty[S, Queue[M]])
      retries <- Ref.of(Map.empty[S, Int])
      skipped <- Ref.of(0)
    } yield new FairRoundRobinDispatcher(
      filter,
      handle,
      queue,
      packets,
      retries,
      skipped,
      maxSourceQueueSize,
      giveUpAfterSkipped,
      dropSourceAfterRetries
    )
}
