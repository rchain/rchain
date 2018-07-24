package coop.rchain.blockstorage

import java.nio.ByteBuffer
import java.nio.file.Path

import scala.collection.JavaConverters._
import scala.language.higherKinds

import cats._
import cats.effect.{ExitCase, Sync}
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.metrics.Metrics
import org.lmdbjava._
import org.lmdbjava.DbiFlags.MDB_CREATE

class LMDBBlockStore[F[_]] private (val env: Env[ByteBuffer], path: Path, blocks: Dbi[ByteBuffer])(
    implicit
    syncF: Sync[F],
    metricsF: Metrics[F])
    extends BlockStore[F] {

  import LMDBBlockStore.MetricNamePrefix

  implicit class RichBlockHash(byteVector: BlockHash) {

    def toDirectByteBuffer: ByteBuffer = {
      val buffer: ByteBuffer = ByteBuffer.allocateDirect(byteVector.size)
      byteVector.copyTo(buffer)
      // TODO: get rid of this:
      buffer.flip()
      buffer
    }
  }

  def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "put")
      ret <- syncF.bracket(syncF.pure(env.txnWrite())) { txn =>
              syncF.delay {
                val (blockHash, blockMessage) = f
                blocks.put(txn,
                           blockHash.toDirectByteBuffer,
                           blockMessage.toByteString.toDirectByteBuffer)
                txn.commit()
              }
            }(txn => syncF.pure(txn.close()))
    } yield ret

  def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "get")
      ret <- syncF.bracket(syncF.pure(env.txnRead()))(txn =>
              syncF.pure {
                val r = Option(blocks.get(txn, blockHash.toDirectByteBuffer)).map(r =>
                  BlockMessage.parseFrom(ByteString.copyFrom(r).newCodedInput()))
                txn.commit()
                r
            })(txn => syncF.pure(txn.close()))
    } yield ret

  def asMap(): F[Map[BlockHash, BlockMessage]] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "as-map")
      ret <- syncF.bracket(syncF.pure(env.txnRead()))(txn =>
              syncF.pure {
                val r = blocks.iterate(txn).asScala.foldLeft(Map.empty[BlockHash, BlockMessage]) {
                  (acc: Map[BlockHash, BlockMessage], x: CursorIterator.KeyVal[ByteBuffer]) =>
                    val hash = ByteString.copyFrom(x.key())
                    val msg  = BlockMessage.parseFrom(ByteString.copyFrom(x.`val`()).newCodedInput())
                    acc.updated(hash, msg)
                }
                txn.commit()
                r
            })(txn => syncF.pure(txn.close()))
    } yield ret

}

object LMDBBlockStore {

  private val MetricNamePrefix = "lmdb-block-store-"

  def create[F[_]](env: Env[ByteBuffer], path: Path)(implicit
                                                     syncF: Sync[F],
                                                     metricsF: Metrics[F]): BlockStore[F] = {

    val blocks: Dbi[ByteBuffer] = env.openDbi(s"blocks", MDB_CREATE)
    new LMDBBlockStore(env, path, blocks)
  }

  def createWithId(env: Env[ByteBuffer], path: Path): BlockStore[Id] = {
    import coop.rchain.metrics.Metrics.MetricsNOP
    val sync: Sync[Id] =
      new Sync[Id] {
        def pure[A](x: A): cats.Id[A] = implicitly[Applicative[Id]].pure(x)

        def handleErrorWith[A](fa: cats.Id[A])(f: Throwable => cats.Id[A]): cats.Id[A] =
          try { fa } catch {
            case e: Exception => f(e)
          }

        def raiseError[A](e: Throwable): cats.Id[A] = throw e

        def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B] =
          implicitly[FlatMap[Id]].flatMap(fa)(f)
        def tailRecM[A, B](a: A)(f: A => cats.Id[Either[A, B]]): cats.Id[B] =
          implicitly[FlatMap[Id]].tailRecM(a)(f)

        def bracketCase[A, B](acquire: A)(use: A => B)(
            release: (A, ExitCase[Throwable]) => Unit): B = {
          val state = acquire
          try {
            use(state)
          } finally {
            release(acquire, ExitCase.Completed)
          }
        }

        def suspend[A](thunk: => A): A = thunk
      }

    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(sync)
    LMDBBlockStore.create(env, path)(sync, metrics)
  }
}
