package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.AttemptOpsF.RichAttempt
import coop.rchain.shared.ByteVectorOps._
import scodec.bits.{BitVector, ByteVector}

trait RootsStore[F[_]] {
  def currentRoot(): F[Option[Blake2b256Hash]]
  def validateAndSetCurrentRoot(key: Blake2b256Hash): F[Option[Blake2b256Hash]]
  def recordRoot(key: Blake2b256Hash): F[Unit]

  def close(): F[Unit]
}

object RootsStoreInstances {
  def rootsStore[F[_]: Sync](store: Store[F]): RootsStore[F] = new RootsStore[F] {
    val tag: ByteBuffer = ByteVector("root".getBytes(StandardCharsets.UTF_8)).toDirectByteBuffer
    val currentRootName: ByteBuffer =
      ByteVector("current-root".getBytes(StandardCharsets.UTF_8)).toDirectByteBuffer

    override def currentRoot(): F[Option[Blake2b256Hash]] =
      for {
        bytes        <- store.get(currentRootName)
        maybeDecoded <- bytes.map(Blake2b256Hash.codecBlake2b256Hash.decode(_).get).sequence
        maybeHash    = maybeDecoded.map(_.value)
      } yield (maybeHash)

    override def validateAndSetCurrentRoot(key: Blake2b256Hash): F[Option[Blake2b256Hash]] =
      for {
        bits    <- Blake2b256Hash.codecBlake2b256Hash.encode(key).get
        bytes   = bits.toByteVector.toDirectByteBuffer
        byteBuf <- store.get(bytes)
        result  <- byteBuf.traverse(_ => store.put(currentRootName, bytes).as(key))
      } yield result

    override def recordRoot(key: Blake2b256Hash): F[Unit] =
      for {
        bits  <- Blake2b256Hash.codecBlake2b256Hash.encode(key).get
        bytes = bits.toByteVector.toDirectByteBuffer
        _     <- store.put(bytes, tag)
        _     <- store.put(currentRootName, bytes)
      } yield ()

    override def close(): F[Unit] = store.close()
  }
}
