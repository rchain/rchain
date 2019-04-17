package coop.rchain.rspace.nextgenrspace.history

import java.nio.ByteBuffer

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.AttemptOps._
import coop.rchain.shared.ByteVectorOps._
import org.lmdbjava.{Dbi, Env}
import scodec.bits.BitVector

class LMDBRootsStore[F[_]: Sync](
    val env: Env[ByteBuffer],
    val dbi: Dbi[ByteBuffer]
) extends AbstractLMDBStore[F]
    with RootsStore[F] {

  val tag: ByteBuffer             = ByteBuffer.wrap("root".getBytes)
  val currentRootName: ByteBuffer = ByteBuffer.wrap("current-root".getBytes)

  override def currentRoot(): F[Option[Blake2b256Hash]] =
    this
      .get(currentRootName)
      .map(Option(_))
      .map(_.map(bytes => Blake2b256Hash.codecBlake2b256Hash.decode(BitVector(bytes)).get.value))

  override def validateRoot(key: Blake2b256Hash): F[Option[Blake2b256Hash]] = {
    val bytes = Blake2b256Hash.codecBlake2b256Hash.encode(key).get.toByteVector.toDirectByteBuffer
    for {
      bytesMaybe <- this.get(bytes).map(Option(_))
      result     = bytesMaybe.map(_ => key)
    } yield result
  }

  override def recordRoot(key: Blake2b256Hash): F[Unit] = {
    val bytes = Blake2b256Hash.codecBlake2b256Hash.encode(key).get.toByteVector.toDirectByteBuffer
    this.put(bytes, tag)
    this.put(currentRootName, bytes)
  }
}
