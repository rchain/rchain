package coop.rchain.rspace.nextgenrspace.history

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.AttemptOps.RichAttempt

trait PointerBlockStore[F[_]] {
  def put(key: Blake2b256Hash, pb: PointerBlock): F[Unit]

  def get(key: Blake2b256Hash): F[Option[PointerBlock]]

  def close(): F[Unit]
}

object PointerBlockStoreInstances {
  def pointerBlockStore[F[_]: Sync](store: Store[F]): PointerBlockStore[F] =
    new PointerBlockStore[F] {
      override def put(key: Blake2b256Hash, pb: PointerBlock): F[Unit] = {
        val bytes = PointerBlock.codecPointerBlock.encode(pb).get
        store.put(key, bytes)
      }

      override def get(key: Blake2b256Hash): F[Option[PointerBlock]] =
        for {
          maybeBytes <- store.get(key)
          result = maybeBytes.map(
            bytes => PointerBlock.codecPointerBlock.decode(bytes).get.value
          )
        } yield result

      override def close(): F[Unit] = store.close()
    }
}
