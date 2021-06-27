package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.AttemptOpsF.RichAttempt
import coop.rchain.shared.ByteVectorOps._
import coop.rchain.store.KeyValueStore
import scodec.bits.{BitVector, ByteVector}
import coop.rchain.shared.syntax._

trait RootsStore[F[_]] {
  def currentRoot(): F[Option[Blake2b256Hash]]
  def validateAndSetCurrentRoot(key: Blake2b256Hash): F[Option[Blake2b256Hash]]
  def recordRoot(key: Blake2b256Hash): F[Unit]

}

object RootsStoreInstances {
  def rootsStore[F[_]: Sync](store: KeyValueStore[F]): RootsStore[F] = new RootsStore[F] {
    val tag: ByteBuffer = ByteVector("root".getBytes(StandardCharsets.UTF_8)).toDirectByteBuffer
    val currentRootName: ByteBuffer =
      ByteVector("current-root".getBytes(StandardCharsets.UTF_8)).toDirectByteBuffer

    override def currentRoot(): F[Option[Blake2b256Hash]] =
      for {
        bytes <- store.get1(
                  currentRootName,
                  identity
                )
        maybeDecoded <- bytes
                         .map(
                           b =>
                             Blake2b256Hash.codecWithBytesStringBlake2b256Hash
                               .decode(BitVector(b))
                               .get
                         )
                         .sequence
        maybeHash = maybeDecoded.map(_.value)
      } yield maybeHash

    override def validateAndSetCurrentRoot(key: Blake2b256Hash): F[Option[Blake2b256Hash]] =
      for {
        bits    <- Blake2b256Hash.codecWithBytesStringBlake2b256Hash.encode(key).get
        bytes   = bits.toByteVector.toDirectByteBuffer
        byteBuf <- store.get1(bytes, identity)
        result <- byteBuf.traverse(
                   _ => store.put1(currentRootName, bytes, identity[ByteBuffer]).as(key)
                 )
      } yield result

    override def recordRoot(key: Blake2b256Hash): F[Unit] =
      for {
        bits  <- Blake2b256Hash.codecWithBytesStringBlake2b256Hash.encode(key).get
        bytes = bits.toByteVector.toDirectByteBuffer
        _     <- store.put1(bytes, tag, identity[ByteBuffer])
        _     <- store.put1(currentRootName, bytes, identity[ByteBuffer])
      } yield ()

  }
}
