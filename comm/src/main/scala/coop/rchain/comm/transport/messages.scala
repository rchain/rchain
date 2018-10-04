package coop.rchain.comm.transport

import java.util.concurrent.TimeoutException

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.FiniteDuration

import coop.rchain.comm.protocol.routing.{Blob, Protocol}

import monix.eval.Task
import monix.execution.Scheduler

trait ServerMessage
// TODO rename to AksMesage and TellMesssage
final case class Ask(msg: Protocol, sender: SenderHandle) extends ServerMessage
final case class Tell(msg: Protocol)                      extends ServerMessage
final case class BlobMessage(blob: Blob)                  extends ServerMessage

trait SenderHandle {
  def reply(msg: CommunicationResponse): Boolean
  def failWith(e: Throwable): Boolean
}

final class ReplyPromise(timeout: FiniteDuration)(implicit scheduler: Scheduler)
    extends SenderHandle {
  private val promise = Promise[CommunicationResponse]

  lazy val future: Future[CommunicationResponse] = {
    val err  = new TimeoutException
    val task = scheduler.scheduleOnce(timeout.length, timeout.unit, () => promise.tryFailure(err))
    promise.future.andThen { case _ => task.cancel() }
  }

  lazy val task: Task[CommunicationResponse] = Task.fromFuture(future)

  def reply(msg: CommunicationResponse): Boolean = promise.trySuccess(msg)

  def failWith(e: Throwable): Boolean = promise.tryFailure(e)
}

object ReplyPromise {
  def apply(timeout: FiniteDuration)(implicit scheduler: Scheduler): ReplyPromise =
    new ReplyPromise(timeout)
}
