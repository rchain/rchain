package coop.rchain.comm.transport

import java.nio.file._

import scala.concurrent.duration._

import cats.implicits._

import PacketOps._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.comm.PeerNode
import coop.rchain.shared.Log

import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber

final case class StreamToPeers(peers: Seq[PeerNode], path: Path, sender: PeerNode)

class StreamObservable(bufferSize: Int, folder: Path)(implicit log: Log[Task], scheduler: Scheduler)
    extends Observable[StreamToPeers] {

  val LogRetryEvery: Int         = 60 // With a delay of 1 second log every minute
  val RetryDelay: FiniteDuration = 1.second

  private val subject = buffer.LimitedBufferObservable.dropNew[StreamToPeers](bufferSize)

  def stream(peers: Seq[PeerNode], blob: Blob): Task[Unit] = {

    val logStreamInformation =
      log.info(s"Streaming packet (type = ${blob.packet.typeId}) to peers ${peers.mkString(", ")}")

    val storeBlob: Task[Option[Path]] =
      blob.packet.store[Task](folder) >>= {
        case Right(file) => Task.pure(Some(file))
        case Left(e)     => log.error(e.message) >> None.pure[Task]
      }

    def push(file: Path): Task[Boolean] =
      Task.delay(subject.pushNext(StreamToPeers(peers, file, blob.sender)))

    def propose(file: Path, retryCount: Int): Task[Unit] =
      push(file) >>= (_.fold(
        log.debug(s"Enqueued for streaming packet $file"),
        retry(file, retryCount)
      ))

    def retry(file: Path, retryCount: Int): Task[Unit] =
      Task
        .defer(logRetry(retryCount) >> propose(file, retryCount + 1))
        .delayExecution(RetryDelay)

    def logRetry(retryCount: Int): Task[Unit] =
      if (retryCount % LogRetryEvery == 0)
        log.warn(s"Client stream message queue is full. Retrying push. Retry $retryCount")
      else Task.unit

    logStreamInformation >> storeBlob >>= (_.fold(Task.unit)(propose(_, 1)))
  }

  def unsafeSubscribeFn(subscriber: Subscriber[StreamToPeers]): Cancelable = {
    val subscription = subject.subscribe(subscriber)
    () => subscription.cancel()
  }
}
