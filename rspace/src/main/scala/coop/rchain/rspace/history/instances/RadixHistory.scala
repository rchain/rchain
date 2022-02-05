package coop.rchain.rspace.history.instances

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.RadixTree._
import coop.rchain.rspace.history._
import scodec.bits.ByteVector

/**
  * History implementation with radix tree
  */
object RadixHistory {
  val emptyRootHash: Blake2b256Hash = Blake2b256Hash.fromByteArray(hashNode(emptyNode)._1.toArray)

  def apply[F[_]: Sync: Parallel](root: Blake2b256Hash, store: RadixStore[F]): F[History[F]] =
    for {
      impl <- Sync[F].delay(new RadixTreeImpl[F](store))
      node <- impl.loadNode(root.bytes, noAssert = true)
    } yield RadixHistory(root, node, impl, store)
}

final case class RadixHistory[F[_]: Sync: Parallel](
    rootHash: Blake2b256Hash,
    rootNode: Node,
    impl: RadixTreeImpl[F],
    store: RadixStore[F]
) extends History[F] {
  override type HistoryF = History[F]

  override def root: Blake2b256Hash = rootHash

  override def reset(root: Blake2b256Hash): F[History[F]] =
    for {
      impl <- Sync[F].delay(new RadixTreeImpl[F](store))
      node <- impl.loadNode(root.bytes, noAssert = true)
    } yield this.copy(root, node, impl, store)

  override def read(key: ByteVector): F[Option[ByteVector]] =
    impl.read(rootNode, key)

  override def process(actions: List[HistoryAction]): F[History[F]] =
    for {
      _ <- Sync[F].ensure(actions.pure[F])(
            new RuntimeException("Cannot process duplicate actions on one key")
          )(hasNoDuplicates)

      newRootNodeOpt <- impl.makeActions(rootNode, actions)
      newRootHash <- newRootNodeOpt match {
                      case Some(newRootNode) =>
                        val hash = impl.saveNode(newRootNode)
                        impl.commit().map(_ => hash.some)
                      case None => none.pure
                    }
      _ <- Sync[F].delay(impl.clearWriteCache())
      _ <- Sync[F].delay(impl.clearReadCache())
    } yield
      if (newRootHash.isDefined)
        this.copy(
          Blake2b256Hash.fromByteArray(newRootHash.get.toArray),
          newRootNodeOpt.get,
          impl,
          store
        )
      else this

  private def hasNoDuplicates(actions: List[HistoryAction]) =
    actions.map(_.key).toSet.size == actions.size
}
