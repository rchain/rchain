package coop.rchain.rspace.history.instances

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.KeyPath
import coop.rchain.rspace.history.RadixTree.{emptyTree, NodeR, RadixTreeImpl, TreeR}
import coop.rchain.rspace.history._
import coop.rchain.shared.Stopwatch
import scodec.bits.ByteVector

import java.util.concurrent.TimeUnit
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.FiniteDuration

/**
  * History implementation with radix tree
  */
final case class RadixHistory[F[_]: Sync: Parallel](
    currentRoot: Blake2b256Hash,
    store: TrieMap[ByteVector, ByteVector]
) extends History[F] {
  val impl     = new RadixTreeImpl(store)
  val rootTree = impl.loadRefWithDefault(currentRoot)

  override def find(key: KeyPath): F[(TriePointer, Vector[Trie])] = ???

  override def root: Blake2b256Hash = currentRoot

  override def reset(root: Blake2b256Hash): History[F] = this.copy(root, store)

  /**
    * Read
    */
  override def read(key: ByteVector): F[Option[ByteVector]] =
    Sync[F].delay(impl.read(rootTree, key))

  /**
    * Write
    */
  override def process(actions: List[HistoryAction]): F[History[F]] =
    for {
      _ <- Sync[F].ensure(actions.pure[F])(
            new RuntimeException("Cannot process duplicate actions on one key")
          )(hasNoDuplicates)

      useParallel = true

      newRootTree <- if (useParallel) processParallel(actions)
                    else Sync[F].delay(processSequential(actions))

      newRootHash = impl.saveRef(newRootTree)
    } yield this.copy(currentRoot = newRootHash, store = store)

//  private def processSequential(actions: List[HistoryAction]): TreeBin = {
//    import scala.Ordering.Implicits.seqDerivedOrdering
//
//    val actionsSorted = actions.sortBy(_.key)
//    val startMs       = System.currentTimeMillis
//    val newRootTree = actionsSorted.foldLeft((rootTree, 0)) {
//      case ((tree, count), action) =>
//        val (prefix, value) = fromAction(action)
//        val newTree         = impl.write(tree, prefix, value)
//
//        val deltaMs  = System.currentTimeMillis - startMs
//        val duration = FiniteDuration(deltaMs, TimeUnit.MILLISECONDS)
//        val durStr   = Stopwatch.showTime(duration)
//        val perMs    = count.toDouble / deltaMs
//        if (count % 25000 == 0) println(s"Progress: $count, elapsed: $durStr, per ms: $perMs")
//
//        (newTree, count + 1)
//    }
//    newRootTree._1
//  }

  private def processSequential(actions: List[HistoryAction]): TreeR = {
    val partitions = actions.groupBy(_.key.head).toList

    partitions.foldLeft(rootTree) {
      case (subTree, (byteIdx, subActions)) =>
        val startMs = System.currentTimeMillis
        val newNode = subActions.foldLeft(subTree) {
          case (subTree, action) =>
            val (prefix, value) = fromAction(action)
            impl.write(subTree, prefix, value)
        }
        val count    = subActions.size
        val deltaMs  = System.currentTimeMillis - startMs
        val duration = FiniteDuration(deltaMs, TimeUnit.MILLISECONDS)
        val durStr   = Stopwatch.showTime(duration)
        val perMs    = count.toDouble / deltaMs
        val idxHex   = ByteVector(byteIdx).toHex
        println(s"Progress ($idxHex): $count, elapsed: $durStr, per ms: $perMs")

        newNode
    }
  }

  private def processParallel(actions: List[HistoryAction]): F[TreeR] =
    for {
      partitions <- Sync[F].delay(actions.groupBy(_.key.head).toList)
      rootNode   = rootTree.asInstanceOf[NodeR]
      newSubNodes = partitions.map {
        case (byteIdx, subActions) =>
          Sync[F].delay {
            val index      = byteIdx & 0xff
            val subNodeRef = rootNode.refs(index)
            val subNode    = if (subNodeRef.nonEmpty) impl.loadRef(subNodeRef) else emptyTree
//            val startMs    = System.currentTimeMillis
            val newNode = subActions.foldLeft(subNode) {
              case (subTree, action) =>
                val (prefix, value) = fromAction(action)
                impl.write(subTree, prefix.tail, value)
            }

//            val count    = subActions.size
//            val deltaMs  = System.currentTimeMillis - startMs
//            val duration = FiniteDuration(deltaMs, TimeUnit.MILLISECONDS)
//            val durStr   = Stopwatch.showTime(duration)
//            val perMs    = count.toDouble / deltaMs
//            val idxHex   = ByteVector(byteIdx).toHex
//            println(s"Progress ($idxHex): $count, elapsed: $durStr, per ms: $perMs")

            (index, newNode)
          }
      }
      updated <- newSubNodes.parSequence
//      updated <- newSubNodes.sequence
      newSubRefs = updated.foldLeft(rootNode.refs) {
        case (childs, (index, subtree)) =>
          val subRef = impl.saveRef(subtree)
          childs.updated(index, subRef.bytes)
      }
      newRootTree = rootNode.copy(refs = newSubRefs)
    } yield newRootTree

  private def fromAction(act: HistoryAction) = act match {
    case InsertAction(key, value) =>
      (ByteVector(key), value.bytes.some)
    case DeleteAction(key) =>
      (ByteVector(key), none)
  }

  private def hasNoDuplicates(actions: List[HistoryAction]) =
    actions.map(_.key).toSet.size == actions.size
}
