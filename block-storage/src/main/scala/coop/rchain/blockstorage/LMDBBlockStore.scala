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
      ret <- syncF.bracketCase(syncF.delay(env.txnWrite())) { txn =>
              syncF.delay {
                val (blockHash, blockMessage) = f
                blocks.put(txn,
                           blockHash.toDirectByteBuffer,
                           blockMessage.toByteString.toDirectByteBuffer)
                txn.commit()
              }
            } {
              case (txn, ExitCase.Completed) => syncF.delay(txn.close())
              case (txn, _)                  => syncF.delay { txn.abort(); txn.close() }
            }
    } yield ret

  def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "get")
      ret <- syncF.bracket(syncF.delay(env.txnRead()))(txn =>
              syncF.delay {
                val r = Option(blocks.get(txn, blockHash.toDirectByteBuffer)).map(r =>
                  BlockMessage.parseFrom(ByteString.copyFrom(r).newCodedInput()))
                txn.commit()
                r
            })(txn => syncF.delay(txn.close()))
    } yield ret

  def asMap(): F[Map[BlockHash, BlockMessage]] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "as-map")
      ret <- syncF.bracket(syncF.delay(env.txnRead()))(txn =>
              syncF.delay {
                val r = blocks.iterate(txn).asScala.foldLeft(Map.empty[BlockHash, BlockMessage]) {
                  (acc: Map[BlockHash, BlockMessage], x: CursorIterator.KeyVal[ByteBuffer]) =>
                    val hash = ByteString.copyFrom(x.key())
                    val msg  = BlockMessage.parseFrom(ByteString.copyFrom(x.`val`()).newCodedInput())
                    acc.updated(hash, msg)
                }
                txn.commit()
                r
            })(txn => syncF.delay(txn.close()))
    } yield ret

  private[blockstorage] def clear(): F[Unit] =
    for {
      ret <- syncF.bracket(syncF.delay(env.txnWrite()))(txn => syncF.delay(blocks.drop(txn)))(txn =>
              syncF.delay(txn.close()))
    } yield ()
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
    import coop.rchain.catscontrib.effect.implicits._
    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(syncId)
    LMDBBlockStore.create(env, path)(syncId, metrics)
  }
}
