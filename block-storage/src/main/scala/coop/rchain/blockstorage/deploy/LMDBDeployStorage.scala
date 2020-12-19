package coop.rchain.blockstorage.deploy

import java.nio.ByteBuffer
import java.nio.file.Path
import cats.syntax.traverse._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.instances.list._
import cats.effect.{Resource, Sync}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.codecs.codecSignedDeployData
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io.{makeDirectory, notExists, IOError}
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.signatures.Signed
import coop.rchain.lmdb.LMDBStore
import coop.rchain.shared.ByteStringOps._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Env, EnvFlags}
import org.lmdbjava.ByteBufferProxy.PROXY_SAFE
import scodec.Attempt.{Failure, Successful}
import scodec.DecodeResult
import scodec.bits.BitVector

import scala.collection.mutable.ArrayBuffer

case object DeployEncodeError extends Exception
case object DeployDecodeError extends Exception

class LMDBDeployStorage[F[_]: Sync] private (storage: LMDBStore[F]) extends DeployStorage[F] {
  def add(deploys: List[Signed[DeployData]]): F[Unit] =
    deploys
      .traverse(
        d =>
          codecSignedDeployData.encode(d) match {
            case Successful(value) =>
              val signature        = d.sig.toDirectByteBuffer
              val serializedDeploy = ByteString.copyFrom(value.toByteArray).toDirectByteBuffer
              (signature -> serializedDeploy).pure[F]
            case Failure(_) =>
              Sync[F].raiseError[(ByteBuffer, ByteBuffer)](DeployEncodeError)
          }
      )
      .flatMap { serializedDeploys =>
        storage.put(serializedDeploys)
      }

  def remove(deploys: List[Signed[DeployData]]): F[Int] =
    storage.delete(deploys.map(_.sig.toDirectByteBuffer))

  def getUnfinalized: F[Set[Signed[DeployData]]] =
    for {
      serializedDeployDataList <- storage.iterate { iterator =>
                                   val arrayBuffer = ArrayBuffer.empty[BitVector]
                                   while (iterator.hasNext) {
                                     val item = iterator.next()
                                     arrayBuffer += BitVector(item.`val`())
                                   }
                                   arrayBuffer.toList
                                 }
      deployDatas <- serializedDeployDataList.traverse { serializedDeployData =>
                      codecSignedDeployData.decode(serializedDeployData) match {
                        case Successful(DecodeResult(value, _)) =>
                          value.pure[F]
                        case Failure(_) =>
                          Sync[F].raiseError[Signed[DeployData]](DeployDecodeError)
                      }
                    }
    } yield deployDatas.toSet

  def close: F[Unit] =
    storage.close()
}

object LMDBDeployStorage {
  final case class Config(
      storagePath: Path,
      mapSize: Long,
      maxDbs: Int = 1,
      maxReaders: Int = 126,
      noTls: Boolean = true
  )

  def make[F[_]: Sync](config: Config): Resource[F, DeployStorage[F]] = {
    implicit val raiseIOError: RaiseIOError[F] = IOError.raiseIOErrorThroughSync[F]

    Resource
      .make[F, LMDBDeployStorage[F]](
        for {
          _ <- notExists[F](config.storagePath).ifM(
                makeDirectory[F](config.storagePath).void,
                ().pure[F]
              )
          env <- Sync[F].delay {
                  val defaultFlags = List(EnvFlags.MDB_NORDAHEAD)
                  val flags        = if (config.noTls) EnvFlags.MDB_NOTLS +: defaultFlags else defaultFlags
                  Env
                    .create(PROXY_SAFE)
                    .setMapSize(config.mapSize)
                    .setMaxDbs(config.maxDbs)
                    .setMaxReaders(config.maxReaders)
                    .open(config.storagePath.toFile, flags: _*)
                }
          dbi <- Sync[F].delay {
                  env.openDbi("deploy_storage", MDB_CREATE)
                }
          lmdbStore = new LMDBStore[F](env, dbi)
        } yield new LMDBDeployStorage[F](lmdbStore)
      )(_.close)
      .widen
  }
}
