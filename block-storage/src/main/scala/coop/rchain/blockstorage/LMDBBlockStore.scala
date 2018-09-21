package coop.rchain.blockstorage

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

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
import org.lmdbjava.Txn.NotReadyException
import coop.rchain.shared.Resources.withResource

class LMDBBlockStore[F[_]] private (val env: Env[ByteBuffer], path: Path, blocks: Dbi[ByteBuffer])(
    implicit
    syncF: Sync[F],
    metricsF: Metrics[F]
) extends BlockStore[F] {

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

  private[this] def withTxn[R](txnThunk: => Txn[ByteBuffer])(f: Txn[ByteBuffer] => R): F[R] =
    syncF.bracketCase(syncF.delay(txnThunk)) { txn =>
      syncF.delay {
        val r = f(txn)
        txn.commit()
        r
      }
    } {
      case (txn, ExitCase.Completed) => syncF.delay(txn.close())
      case (txn, _) =>
        syncF.delay {
          try {
            txn.abort()
          } catch {
            case ex: NotReadyException =>
              ex.printStackTrace()
              TxnOps.manuallyAbortTxn(txn)
            // vide: rchain/rspace/src/main/scala/coop/rchain/rspace/LMDBOps.scala
          }
          txn.close()
        }
    }

  private[this] def withWriteTxn(f: Txn[ByteBuffer] => Unit): F[Unit] =
    withTxn(env.txnWrite())(f)

  private[this] def withReadTxn[R](f: Txn[ByteBuffer] => R): F[R] =
    withTxn(env.txnRead())(f)

  def put(f: => (BlockHash, BlockMessage)): F[Unit] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "put")
      ret <- withWriteTxn { txn =>
              val (blockHash, blockMessage) = f
              blocks.put(
                txn,
                blockHash.toDirectByteBuffer,
                blockMessage.toByteString.toDirectByteBuffer
              )
            }
    } yield ret

  def get(blockHash: BlockHash): F[Option[BlockMessage]] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "get")
      ret <- withReadTxn { txn =>
              Option(blocks.get(txn, blockHash.toDirectByteBuffer))
                .map(r => BlockMessage.parseFrom(ByteString.copyFrom(r).newCodedInput()))
            }
    } yield ret

  override def find(p: BlockHash => Boolean): F[Seq[(BlockHash, BlockMessage)]] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "find")
      ret <- withReadTxn { txn =>
              withResource(blocks.iterate(txn)) { iterator =>
                iterator.asScala
                  .map(kv => (ByteString.copyFrom(kv.key()), kv.`val`()))
                  .withFilter { case (key, _) => p(key) }
                  .map {
                    case (key, value) =>
                      val msg = BlockMessage.parseFrom(ByteString.copyFrom(value).newCodedInput())
                      (key, msg)
                  }
                  .toList
              }
            }
    } yield ret

  @deprecated(
    message = "to be removed when casper code no longer needs the whole DB in memmory",
    since = "0.5"
  )
  def asMap(): F[Map[BlockHash, BlockMessage]] =
    for {
      _ <- metricsF.incrementCounter(MetricNamePrefix + "as-map")
      ret <- withReadTxn { txn =>
              blocks.iterate(txn).asScala.foldLeft(Map.empty[BlockHash, BlockMessage]) {
                (acc: Map[BlockHash, BlockMessage], x: CursorIterator.KeyVal[ByteBuffer]) =>
                  val hash = ByteString.copyFrom(x.key())
                  val msg  = BlockMessage.parseFrom(ByteString.copyFrom(x.`val`()).newCodedInput())
                  acc.updated(hash, msg)
              }
            }
    } yield ret

  def clear(): F[Unit] =
    for {
      ret <- withWriteTxn { txn =>
              blocks.drop(txn)
            }
    } yield ()

  override def close(): F[Unit] =
    syncF.delay { env.close() }
}

object LMDBBlockStore {

  private val MetricNamePrefix = "lmdb-block-store-"

  case class Config(
      path: Path,
      mapSize: Long,
      maxDbs: Int = 1,
      maxReaders: Int = 126,
      noTls: Boolean = true
  )

  def create[F[_]](config: Config)(
      implicit
      syncF: Sync[F],
      metricsF: Metrics[F]
  ): LMDBBlockStore[F] = {
    if (Files.notExists(config.path)) Files.createDirectories(config.path)

    val flags = if (config.noTls) List(EnvFlags.MDB_NOTLS) else List.empty
    val env = Env
      .create()
      .setMapSize(config.mapSize)
      .setMaxDbs(config.maxDbs)
      .setMaxReaders(config.maxReaders)
      .open(config.path.toFile, flags: _*) //TODO this is a bracket

    val blocks: Dbi[ByteBuffer] = env.openDbi(s"blocks", MDB_CREATE) //TODO this is a bracket
    new LMDBBlockStore(env, config.path, blocks)
  }

  def create[F[_]](env: Env[ByteBuffer], path: Path)(
      implicit
      syncF: Sync[F],
      metricsF: Metrics[F]
  ): BlockStore[F] = {
    val blocks: Dbi[ByteBuffer] = env.openDbi(s"blocks", MDB_CREATE)
    new LMDBBlockStore[F](env, path, blocks)
  }

  def createWithId(env: Env[ByteBuffer], path: Path): BlockStore[Id] = {
    import coop.rchain.metrics.Metrics.MetricsNOP
    import coop.rchain.catscontrib.effect.implicits._
    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(syncId)
    LMDBBlockStore.create(env, path)(syncId, metrics)
  }
}
