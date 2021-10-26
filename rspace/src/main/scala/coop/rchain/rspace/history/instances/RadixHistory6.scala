package coop.rchain.rspace.history.instances

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.KeyPath
import coop.rchain.rspace.history.RadixTree6._
import coop.rchain.rspace.history._
import scodec.bits.ByteVector

import scala.language.higherKinds

/**
  * History implementation with radix tree
  */
final case class RadixHistory6[F[_]: Sync: Parallel](
    currentRoot: Blake2b256Hash,
    store: RadixStore[F]
) extends History[F] {
  val impl              = new RadixTree6Impl[F](store, true)
  val rootNode: F[Node] = impl.loadNode(currentRoot.bytes, noAssert = true)

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

      funNum = 4
      n      <- rootNode
      newRootNode <- funNum match {
                      case 1 => processSequential(n, actions)
                      case 2 => processParallel(actions)
                      case 3 => processParallel3(n, actions)
                      case 4 => processParallel4(n, actions)
                      case 5 => processParallel5(n, actions).map(_.get)
                    }

      newRootHash <- if (newRootNode != emptyNode) impl.saveNode(newRootNode)
                    else Sync[F].delay(History.emptyRootHash) // todo fix StorageActionsTests
      _ <- impl.commit
      _ <- impl.clearPutCache
      _ <- impl.clearGetCache
    } yield this.copy(newRootHash, store)

  private def processSequential(startNode: Node, actions: List[HistoryAction]): F[Node] =
    actions.foldLeft(Sync[F].delay(startNode)) {
      case (fTempNode, action) =>
        val (prefix, value) = fromAction(action)
        for {
          tempNode <- fTempNode
          result   <- impl.write(tempNode, prefix, value)
        } yield result
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
                                    ByteVector.empty
                                  )
                        } yield tempRootNode.updated(index, child)
                    }
    } yield newRootNode

  private def processParallel3(curNode: Node, curActions: List[HistoryAction]): F[Node] = {
    val groups = curActions.groupBy(_.key.head).toList
    val newSubNodes = groups.map {
      case (groupIdx, groupActions) =>
        val index    = impl.toInt(groupIdx)
        val fSubNode = impl.createNodeFromChild(curNode(index))
        if (groupActions.length == 1)
          for {
            subNode         <- fSubNode
            (prefix, value) = fromAction(groupActions.head)
            newNode         <- impl.write(subNode, prefix.tail, value)
          } yield (index, newNode)
        else {
          val firstInsertAction                 = groupActions.collect { case v: InsertAction => v }.take(1)
          val existInsertActionInGroup: Boolean = if (firstInsertAction.length == 1) true else false
          for {
            subNode <- fSubNode
            clearGroupActions = if (subNode == emptyNode && !existInsertActionInGroup)
              groupActions.collect { case v: InsertAction => v } else groupActions
            newActions = clearGroupActions.map {
              case InsertAction(key, hash) => InsertAction(key.tail, hash)
              case DeleteAction(key)       => DeleteAction(key.tail)
            }
            newNode <- processParallel3(subNode, newActions)
          } yield (index, newNode)
        }
    }
    for {
      updated <- newSubNodes.parSequence
      newRootNode <- updated.foldLeft(Sync[F].pure(curNode)) {
                      case (ftempNode, (index, newSubNode)) =>
                        for {
                          tempNode <- ftempNode
                          child <- impl.saveNodeAndCreateChild(
                                    newSubNode,
                                    ByteVector.empty
                                  )
                        } yield tempNode.updated(index, child)
                    }
    } yield newRootNode
  }

  private def processParallel4(curNode: Node, curActions: List[HistoryAction]): F[Node] = {
    val groups = curActions.groupBy(_.key.head).toList
    val newGroupChilds = groups.map {
      case (groupIdx, groupActions) =>
        val index = impl.toInt(groupIdx)
        val child = curNode(index)
        if (groupActions.length == 1) {
          val (prefix, value) = fromAction(groupActions.head)
          for {
            newChild <- impl.writeInChild(child, prefix.tail, value)
          } yield (index, newChild)
        } else {
          val firstInsertAction                 = groupActions.collect { case v: InsertAction => v }.take(1)
          val existInsertActionInGroup: Boolean = if (firstInsertAction.length == 1) true else false
          val clearGroupActions =
            if (child == EmptyChild && !existInsertActionInGroup)
              groupActions.collect { case v: InsertAction => v } else groupActions
          val newActions = clearGroupActions.map {
            case InsertAction(key, hash) => InsertAction(key.tail, hash)
            case DeleteAction(key) =>
              DeleteAction(key.tail)
          }
          for {
            createdNode <- impl.createNodeFromChild(curNode(index))
            newNode     <- processParallel4(createdNode, newActions)
            newChild    <- impl.saveNodeAndCreateChild(newNode, ByteVector.empty)
          } yield (index, newChild)
        }
    }
    for {
      updated <- newGroupChilds.parSequence
      newCurNode = updated.foldLeft(curNode) {
        case (tempNode, (index, newChild)) =>
          tempNode.updated(index, newChild)
      }
    } yield newCurNode
  }

  private def processParallel5(curNode: Node, curActions: List[HistoryAction]): F[Option[Node]] = {
    val groups = curActions.groupBy(_.key.head).toList
    val newGroupChildrenF = groups.map {
      case (groupIdx, groupActions) =>
        val index = impl.toInt(groupIdx)
        val child = curNode(index)
        if (groupActions.length == 1) {
          val (prefix, value) = fromAction(groupActions.head)
          impl.writeInChild2(child, prefix.tail, value).map((index, _))
        } else {
          val notExistInsertAction = groupActions.collectFirst { case _: InsertAction => true }.isEmpty
          val clearGroupActions =
            if (child == EmptyChild && notExistInsertAction) groupActions.collect {
              case v: InsertAction => v
            } else groupActions
          val newActions = clearGroupActions.map { //todo rewrite with common HistoryAction.key
            case InsertAction(key, hash) => InsertAction(key.tail, hash)
            case DeleteAction(key)       => DeleteAction(key.tail)
          }
          for {
            createdNode <- impl.createNodeFromChild(curNode(index))
            newNodeOpt  <- processParallel5(createdNode, newActions)
            newChild    <- newNodeOpt.traverse(impl.saveNodeAndCreateChild(_, ByteVector.empty))
          } yield (index, newChild)
        }
    }
    for {
      newGroupChildren <- newGroupChildrenF.parSequence
      newCurNode = newGroupChildren.foldLeft(curNode) {
        case (tempNode, (index, newChildOpt)) =>
          newChildOpt.map(tempNode.updated(index, _)).getOrElse(tempNode)
      }
    } yield if (newCurNode != curNode) newCurNode.some else none
  }

  private def fromAction(act: HistoryAction) = act match {
    case InsertAction(key, value) =>
      (ByteVector(key), value.bytes.some)
    case DeleteAction(key) =>
      (ByteVector(key), none)
  }

  private def hasNoDuplicates(actions: List[HistoryAction]) =
    actions.map(_.key).toSet.size == actions.size
}
