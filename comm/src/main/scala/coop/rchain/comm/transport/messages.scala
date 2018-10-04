package coop.rchain.comm.transport

import java.util.concurrent.atomic.AtomicBoolean

import coop.rchain.comm.protocol.routing.{Blob, Protocol}

import monix.eval.Callback

trait ServerMessage
// TODO rename to AksMesage and TellMesssage
final case class Ask(msg: Protocol, sender: SenderHandle) extends ServerMessage
final case class Tell(msg: Protocol)                      extends ServerMessage
final case class BlobMessage(blob: Blob)                  extends ServerMessage

trait SenderHandle {
  def reply(msg: CommunicationResponse): Unit
  def failWith(e: Throwable): Unit
  def complete: Boolean
}

final class Reply(callback: Callback[CommunicationResponse]) extends SenderHandle {
  // contract: the callback can be called only once
  private val called = new AtomicBoolean(false)

  def reply(msg: CommunicationResponse): Unit =
    if (!called.getAndSet(true)) {
      callback.onSuccess(msg)
    }

  def failWith(e: Throwable): Unit =
    if (!called.getAndSet(true)) {
      callback.onError(e)
    }

  def complete: Boolean = called.get()
}

object Reply {
  def apply(callback: Callback[CommunicationResponse]): Reply = new Reply(callback)
}
