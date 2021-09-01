package coop.rchain.rspace.history.instances

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.KeyPath
import coop.rchain.rspace.history.RadixTree3._
import coop.rchain.rspace.history._
import scodec.bits.ByteVector

import scala.collection.concurrent.TrieMap

/**
  * History implementation with radix tree
  */
final case class RadixHistory3[F[_]: Sync: Parallel](
    currentRoot: Blake2b256Hash,
    store: TrieMap[ByteVector, ByteVector]
) extends History[F] {
  val impl     = new RadixTreeImpl(store)
  val rootNode = impl.loadNodeWithDefault(currentRoot)

  override def find(key: KeyPath): F[(TriePointer, Vector[Trie])] = ???

  override def root: Blake2b256Hash = currentRoot

  override def reset(root: Blake2b256Hash): History[F] = this.copy(root, store)

  /**
    * Read
    */
  override def read(key: ByteVector): F[Option[ByteVector]] =
    Sync[F].delay(impl.read(rootNode, key))

  /**
    * Write
    */
  override def process(actions: List[HistoryAction]): F[History[F]] =
    for {
      _ <- Sync[F].ensure(actions.pure[F])(
            new RuntimeException("Cannot process duplicate actions on one key")
          )(hasNoDuplicates)

      funNum = 2
      newRootNode <- funNum match {
                      case 0 => Sync[F].delay(processOneSequential(actions))
                      case 1 => Sync[F].delay(processSequential(actions))
                      case 2 => processParallel(actions)
                    }

      newRootHash = impl
        .saveNode(newRootNode)

    } yield this.copy(
      currentRoot = if (newRootNode != emptyNode) newRootHash else History.emptyRootHash, // todo: magic value for StorageActionsTests
      store = store
    )

  private def processOneSequential(actions: List[HistoryAction]): Vector[Child] =
    actions.foldLeft(rootNode) {
      case (tempNode, action) =>
        val (prefix, value) = fromAction(action)
        impl.write(tempNode, prefix, value)
    }

  private def processSequential(actions: List[HistoryAction]): Vector[Child] = {
    val partitions = actions.groupBy(_.key.head).toList
    partitions.foldLeft(rootNode) {
      case (tempNode, (_, subActions)) =>
        val newNode = subActions.foldLeft(tempNode) {
          case (tempNode2, action) =>
            val (prefix, value) = fromAction(action)
            impl.write(tempNode2, prefix, value)
        }
        newNode
    }
  }

  private def processParallel(actions: List[HistoryAction]): F[Vector[Child]] =
    for {
      partitions <- Sync[F].delay(actions.groupBy(_.key.head).toList)
      newSubNodes = partitions.map {
        case (idx, subActions) =>
          Sync[F].delay {
            val index   = idx & 0xff
            val subNode = impl.createNodeFromChild(rootNode(index))
            val newNode = subActions.foldLeft(subNode) {
              case (tempSubNode, action) =>
                val (prefix, value) = fromAction(action)
                impl.write(tempSubNode, prefix.tail, value)
            }
            (index, newNode)
          }
      }
      updated <- newSubNodes.parSequence
      newRootNode = updated.foldLeft(rootNode) {
        case (tempRootNode, (index, newSubNode)) =>
          tempRootNode.updated(index, impl.createChildFromNode(newSubNode, ByteVector.empty))
      }
//      newRootTree = rootNode.copy(newRootNode) todo: it's normal?
    } yield newRootNode

  private def fromAction(act: HistoryAction) = act match {
    case InsertAction(key, value) =>
      (ByteVector(key), value.bytes.some)
    case DeleteAction(key) =>
      (ByteVector(key), none)
  }

  private def hasNoDuplicates(actions: List[HistoryAction]) =
    actions.map(_.key).toSet.size == actions.size
}
