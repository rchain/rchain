package coop.rchain.blockstorage

import cats._
import cats.effect.{Bracket, ExitCase}
import cats.implicits._
import cats.mtl.MonadState
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.metrics.Metrics

import scala.language.higherKinds
import scala.collection.JavaConverters._

import java.nio.ByteBuffer
import java.nio.file.Path
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._

class LMDBBlockStore[F[_]] private (val env: Env[ByteBuffer], path: Path, blocks: Dbi[ByteBuffer])(
    implicit
    val applicative: Applicative[F],
    bracketF: Bracket[F, Exception],
    metricsF: Metrics[F])
    extends BlockStore[F] {

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
      _           <- metricsF.incrementCounter("block-store-put")
      applicative = bracketF
      ret <- bracketF.bracket(applicative.pure(env.txnWrite())) { txn =>
              val (blockHash, blockMessage) = f
              applicative.pure {
                blocks.put(txn,
                           blockHash.toDirectByteBuffer,
                           blockMessage.toByteString.toDirectByteBuffer)
                txn.commit()
              }
            }(txn => applicative.pure(txn.close()))
    } yield ret

  def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    for {
      _           <- metricsF.incrementCounter("block-store-get")
      applicative = bracketF
      ret <- bracketF.bracket(applicative.pure(env.txnRead()))(txn =>
              applicative.pure {
                val r = Option(blocks.get(txn, blockHash.toDirectByteBuffer)).map(r =>
                  BlockMessage.parseFrom(ByteString.copyFrom(r).newCodedInput()))
                txn.commit()
                r
            })(txn => applicative.pure(txn.close()))
    } yield ret

  def asMap(): F[Map[BlockHash, BlockMessage]] =
    for {
      _           <- metricsF.incrementCounter("block-store-as-map")
      applicative = bracketF
      ret <- bracketF.bracket(applicative.pure(env.txnRead()))(txn =>
              applicative.pure {
                val r = blocks.iterate(txn).asScala.foldLeft(Map.empty[BlockHash, BlockMessage]) {
                  (acc: Map[BlockHash, BlockMessage], x: CursorIterator.KeyVal[ByteBuffer]) =>
                    val hash = ByteString.copyFrom(x.key())
                    val msg  = BlockMessage.parseFrom(ByteString.copyFrom(x.`val`()).newCodedInput())
                    acc.updated(hash, msg)
                }
                txn.commit()
                r
            })(txn => applicative.pure(txn.close()))
    } yield ret

}

object LMDBBlockStore {
  def create[F[_]](env: Env[ByteBuffer], path: Path)(implicit
                                                     bracketF: Bracket[F, Exception],
                                                     metricsF: Metrics[F]): BlockStore[F] = {

    val blocks: Dbi[ByteBuffer] = env.openDbi(s"blocks", MDB_CREATE)
    new LMDBBlockStore(env, path, blocks)
  }

  def createWithId(env: Env[ByteBuffer], path: Path): BlockStore[Id] = {
    import coop.rchain.metrics.Metrics.MetricsNOP
    implicit val bracket: Bracket[Id, Exception] =
      new Bracket[Id, Exception] {
        def pure[A](x: A): cats.Id[A] = implicitly[Applicative[Id]].pure(x)

        // Members declared in cats.ApplicativeError
        def handleErrorWith[A](fa: cats.Id[A])(f: Exception => cats.Id[A]): cats.Id[A] =
          ??? //implicitly[ApplicativeError[Id, Exception]].handleErrorWith(fa)(f)

        def raiseError[A](e: Exception): cats.Id[A] = ???
        //implicitly[ApplicativeError[Id, Exception]].raiseError(e)

        // Members declared in cats.FlatMap
        def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B] =
          implicitly[FlatMap[Id]].flatMap(fa)(f)
        def tailRecM[A, B](a: A)(f: A => cats.Id[Either[A, B]]): cats.Id[B] =
          implicitly[FlatMap[Id]].tailRecM(a)(f)

        def bracketCase[A, B](acquire: A)(use: A => B)(
            release: (A, ExitCase[Exception]) => Unit): B = {
          val state = acquire
          try {
            use(state)
          } finally {
            release(acquire, ExitCase.Completed)
          }
        }

      }
    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(bracket)
    LMDBBlockStore.create(env, path)(bracket, metrics)
  }
}
