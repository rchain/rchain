package coop.rchain.rspace.nextgenrspace.history

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import cats.Applicative
import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.AttemptOps._
import coop.rchain.shared.ByteVectorOps._
import scodec.bits.{BitVector, ByteVector}

trait RootsStore[F[_]] {
  def currentRoot(): F[Option[Blake2b256Hash]]
  def validateRoot(key: Blake2b256Hash): F[Unit]
  def recordRoot(key: Blake2b256Hash): F[Unit]

  def close(): F[Unit]
}

object RootsStoreInstances {
  val unknownRoot = new RuntimeException("unknown root")

  def rootsStore[F[_]: Sync](store: Store[F]): RootsStore[F] = new RootsStore[F] {
    val tag: ByteBuffer = ByteVector("root".getBytes(StandardCharsets.UTF_8)).toDirectByteBuffer
    val currentRootName: ByteBuffer =
      ByteVector("current-root".getBytes(StandardCharsets.UTF_8)).toDirectByteBuffer

    override def currentRoot(): F[Option[Blake2b256Hash]] =
      store
        .get(currentRootName)
        .map(_.map(bytes => Blake2b256Hash.codecBlake2b256Hash.decode(BitVector(bytes)).get.value))

    override def validateRoot(key: Blake2b256Hash): F[Unit] = {
      val bytes = Blake2b256Hash.codecBlake2b256Hash.encode(key).get.toByteVector.toDirectByteBuffer
      (for {
        bytesMaybe <- store.get(bytes)
        result     <- bytesMaybe.traverse(_ => store.put(currentRootName, bytes).map(_ => key))
      } yield result).flatMap {
        case None => Sync[F].raiseError[Unit](unknownRoot)
        case _    => Applicative[F].pure(())
      }
    }

    override def recordRoot(key: Blake2b256Hash): F[Unit] = {
      val bytes = Blake2b256Hash.codecBlake2b256Hash.encode(key).get.toByteVector.toDirectByteBuffer
      for {
        _ <- store.put(bytes, tag)
        _ <- store.put(currentRootName, bytes)
      } yield ()
    }

    override def close(): F[Unit] = store.close()
  }
}
