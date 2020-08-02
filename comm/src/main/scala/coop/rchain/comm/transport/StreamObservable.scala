package coop.rchain.comm.transport

import java.nio.file._

import cats.implicits._

import PacketOps._
import coop.rchain.comm.PeerNode
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps._

import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber

final case class Stream(path: Path, sender: PeerNode)

class StreamObservable(peer: PeerNode, bufferSize: Int, folder: Path)(
    implicit log: Log[Task],
    scheduler: Scheduler
) extends Observable[Stream] {

  private val subject = buffer.LimitedBufferObservable.dropNew[Stream](bufferSize)

  def enque(blob: Blob): Task[Unit] = {

    val logStreamInformation =
      log.debug(
        s"Pushing message to $peer stream message queue."
      )

    val storeBlob: Task[Option[Path]] =
      blob.packet.store[Task](folder) >>= {
        case Right(file) => Task.pure(Some(file))
        case Left(e)     => log.error(e.message) >> None.pure[Task]
      }

    def push(file: Path): Task[Boolean] =
      Task.delay(subject.pushNext(Stream(file, blob.sender)))

    def propose(file: Path): Task[Unit] = {
      val processError = log.warn(
        s"Client stream message queue for $peer is full (${bufferSize} items). Dropping message.)"
      ) >> file.deleteSingleFile[Task]
      push(file) >>= (pushSucceed => processError.unlessA(pushSucceed))
    }

    logStreamInformation >> storeBlob >>= (_.fold(Task.unit)(propose))
  }

  def unsafeSubscribeFn(subscriber: Subscriber[Stream]): Cancelable = {
    val subscription = subject.subscribe(subscriber)
    () => subscription.cancel()
  }
}
