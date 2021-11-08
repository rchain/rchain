package coop.rchain.rspace.history.instances

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.KeyPath
import coop.rchain.rspace.history.RadixTree8._
import coop.rchain.rspace.history._
import scodec.bits.ByteVector

import scala.language.higherKinds

/**
  * History implementation with radix tree
  */
final case class RadixHistory8[F[_]: Sync: Parallel](
    currentRoot: Blake2b256Hash,
    store: RadixStore[F]
) extends History[F] {
  val impl               = new RadixTreeImpl[F](store)
  val rootNodeF: F[Node] = impl.loadNode(currentRoot.bytes, noAssert = true)

  override def find(key: KeyPath): F[(TriePointer, Vector[Trie])] = ???

  override def root: Blake2b256Hash = currentRoot

  override def reset(root: Blake2b256Hash): History[F] = this.copy(root, store)

  override def read(key: ByteVector): F[Option[ByteVector]] =
    for {
      rootNode <- rootNodeF
      data     <- impl.read(rootNode, key)
    } yield data

  override def process(actions: List[HistoryAction]): F[History[F]] =
    for {
      _ <- Sync[F].ensure(actions.pure[F])(
            new RuntimeException("Cannot process duplicate actions on one key")
          )(hasNoDuplicates)

      rootNode       <- rootNodeF
      newRootNodeOpt <- impl.makeActions(rootNode, actions)
      newRootHash <- newRootNodeOpt match {
                      case Some(newRootNode) =>
                        val hash = impl.saveNode(newRootNode)
                        impl.commit().map(_ => hash.some)
                      case None => none.pure
                    }
      _ <- Sync[F].delay(impl.clearPutCache())
      _ <- Sync[F].delay(impl.clearCacheR())
    } yield if (newRootHash.isDefined) this.copy(newRootHash.get, store) else this

  private def hasNoDuplicates(actions: List[HistoryAction]) =
    actions.map(_.key).toSet.size == actions.size
}
