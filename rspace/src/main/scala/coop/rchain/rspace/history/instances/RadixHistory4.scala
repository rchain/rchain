package coop.rchain.rspace.history.instances

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.KeyPath
import coop.rchain.rspace.history.RadixTree4._
import coop.rchain.rspace.history._
import scodec.bits.ByteVector

import scala.language.higherKinds

/**
  * History implementation with radix tree
  */
final case class RadixHistory4[F[_]: Sync: Parallel](
    currentRoot: Blake2b256Hash,
    store: RadixStore[F]
) extends History[F] {
  val impl              = new RadixTree4Impl[F](store)
  val rootNode: F[Node] = impl.loadNodeWithDefault(currentRoot)

  override def find(key: KeyPath): F[(TriePointer, Vector[Trie])] = ???

  override def root: Blake2b256Hash = currentRoot

  override def reset(root: Blake2b256Hash): History[F] = this.copy(root, store)

  /**
    * Read
    */
  override def read(key: ByteVector): F[Option[ByteVector]] =
    for {
      n    <- rootNode
      data <- impl.read(n, key)
    } yield data

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
                      case 0 => processOneSequential(actions)
                      case 1 => processSequential(actions)
                      case 2 => processParallel(actions)
                    }
      newRootHash <- if (newRootNode != emptyNode) impl.saveNode(newRootNode)
                    else Sync[F].delay(History.emptyRootHash) // todo fix StorageActionsTests
    } yield this.copy(newRootHash, store)

  private def processOneSequential(actions: List[HistoryAction]): F[Node] =
    actions.foldLeft(rootNode) {
      case (fTempNode, action) =>
        val (prefix, value) = fromAction(action)
        for {
          tempNode <- fTempNode
          result   <- impl.write(tempNode, prefix, value)
        } yield result
    }

  private def processSequential(actions: List[HistoryAction]): F[Node] = {
    val partitions = actions.groupBy(_.key.head).toList
    partitions.foldLeft(rootNode) {
      case (tempNode, (_, subActions)) =>
        val newNode = subActions.foldLeft(tempNode) {
          case (fTempNode2, action) =>
            val (prefix, value) = fromAction(action)
            for {
              tempNode2 <- fTempNode2
              result    <- impl.write(tempNode2, prefix, value)
            } yield result
        }
        newNode
    }
  }

  private def processParallel(actions: List[HistoryAction]): F[Node] =
    for {
      partitions <- Sync[F].delay(actions.groupBy(_.key.head).toList)
      newSubNodes = partitions.map {
        case (idx, subActions) =>
          for {
            index   <- Sync[F].delay(idx & 0xff)
            root    <- rootNode
            subNode = impl.createNodeFromChild(root(index))
            newNode <- subActions.foldLeft(subNode) {
                        case (ftempSubNode, action) =>
                          val (prefix, value) = fromAction(action)
                          for {
                            tempSubNode <- ftempSubNode
                            result      <- impl.write(tempSubNode, prefix.tail, value)
                          } yield result
                      }
          } yield (index, newNode)
      }
      updated <- newSubNodes.parSequence
      newRootNode <- updated.foldLeft(rootNode) {
                      case (ftempRootNode, (index, newSubNode)) =>
                        for {
                          tempRootNode <- ftempRootNode
                          child <- impl.saveNodeAndCreateChild(
                                    newSubNode,
                                    ByteVector.empty,
                                    optimization = true
                                  )
                        } yield tempRootNode.updated(index, child)
                    }
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
