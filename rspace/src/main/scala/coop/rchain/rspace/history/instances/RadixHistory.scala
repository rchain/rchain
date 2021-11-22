package coop.rchain.rspace.history.instances

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.KeyPath
import coop.rchain.rspace.history.RadixTree._
import coop.rchain.rspace.history._
import scodec.bits.ByteVector

import scala.language.higherKinds

/**
  * History implementation with radix tree
  */
object RadixHistory {
  def apply[F[_]: Sync: Parallel](
      rootHash: Blake2b256Hash,
      store: RadixStore[F]
  ): F[RadixHistory[F]] =
    for {
      impl     <- Sync[F].delay(new RadixTreeImpl[F](store))
      rootNode <- impl.loadNode(rootHash.bytes, noAssert = true)
    } yield RadixHistory(rootHash, rootNode, impl, store)
}

final case class RadixHistory[F[_]: Sync: Parallel](
    rootHash: Blake2b256Hash,
    rootNode: Node,
    impl: RadixTreeImpl[F],
    store: RadixStore[F]
) extends History[F] {

  override def find(key: KeyPath): F[(TriePointer, Vector[Trie])] = ???

  override def root: Blake2b256Hash = rootHash

  override def reset(root: Blake2b256Hash): History[F] =
    this.copy(rootHash, rootNode, impl, store)

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
