package coop.rchain.rspace.history.instances

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree._
import coop.rchain.rspace.history._
import coop.rchain.shared.syntax.sharedSyntaxKeyValueStore
import coop.rchain.store.{KeyValueStore, KeyValueTypedStore}
import scodec.bits.ByteVector

/**
  * History implementation with radix tree
  */
object RadixHistory {
  val emptyRootHash: Blake2b256Hash = RadixTree.emptyRootHash

  def apply[F[_]: Sync: Parallel](
      root: Blake2b256Hash,
      store: KeyValueTypedStore[F, ByteVector, ByteVector]
  ): F[RadixHistory[F]] =
    for {
      impl <- Sync[F].delay(new RadixTreeImpl[F](store))
      node <- impl.loadNode(root, noAssert = true)
    } yield RadixHistory(root, node, impl, store)

  def createStore[F[_]: Sync](
      store: KeyValueStore[F]
  ): KeyValueTypedStore[F, ByteVector, ByteVector] =
    store.toTypedStore(scodec.codecs.bytes, scodec.codecs.bytes)
}

final case class RadixHistory[F[_]: Sync: Parallel](
    rootHash: Blake2b256Hash,
    rootNode: Node,
    impl: RadixTreeImpl[F],
    store: KeyValueTypedStore[F, ByteVector, ByteVector]
) extends History[F] {
  override type HistoryF = History[F]

  override def root: Blake2b256Hash = rootHash

  override def reset(root: Blake2b256Hash): F[History[F]] =
    for {
      impl <- Sync[F].delay(new RadixTreeImpl[F](store))
      node <- impl.loadNode(root, noAssert = true)
    } yield this.copy(root, node, impl, store)

  override def read(key: ByteVector): F[Option[Blake2b256Hash]] =
    impl.read(rootNode, key)

  override def process(actions: List[HistoryAction]): F[History[F]] =
    for {

      /** TODO: To improve time, it is possible to implement this check into the [[RadixTreeImpl.saveAndCommit()]]. */
      _ <- new RuntimeException("Cannot process duplicate actions on one key.").raiseError
            .unlessA(hasNoDuplicates(actions))

      newRootDataOpt <- impl.saveAndCommit(rootNode, actions)
      newHistoryOpt = newRootDataOpt.map { newRootData =>
        val (newRootNode, newRootHash) = newRootData
        this.copy(newRootHash, newRootNode, impl, store)
      }
      _ = impl.clearReadCache()
    } yield newHistoryOpt.getOrElse(this)

  private def hasNoDuplicates(actions: List[HistoryAction]) =
    actions.map(_.key).distinct.size == actions.size
}
