package coop.rchain.node

import cats.syntax.all._
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.{BindPattern, ListParWithRandom}
import coop.rchain.rspace.Match
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.{
  EmptyPointer,
  EmptyTrie,
  HistoryStore,
  HistoryStoreInstances,
  LeafPointer,
  NodePointer,
  NonEmptyTriePointer,
  PointerBlock,
  Skip,
  SkipPointer,
  Trie,
  TriePointer,
  ValuePointer
}
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import monix.eval.Task
import org.rogach.scallop.ScallopConf
import monix.execution.Scheduler.Implicits.global

import java.nio.file.Path

final case class CompareOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "state-balance-main"

  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )
  val stateHash1 = opt[String](
    descr = s"State hash 1",
    required = true
  )
  val stateHash2 = opt[String](
    descr = s"State hash 2",
    required = true
  )

  verify()

}

object TrieCompare {
  def main(args: Array[String]): Unit = {
    val options    = CompareOptions(args)
    val dataDir    = options.dataDir()
    val stateHash1 = options.stateHash1()
    val stateHash2 = options.stateHash2()

    final case class Result(t: List[(Trie, Trie)], tp: List[(TriePointer, TriePointer)])

    import coop.rchain.rholang.interpreter.storage._
    implicit val log: Log[Task] = Log.log
    def compare(s1: Blake2b256Hash, s2: Blake2b256Hash)(
        implicit hs: HistoryStore[Task]
    ): Task[Either[List[(TriePointer, TriePointer)], (Trie, Trie)]] =
      for {
        t1 <- hs.get(s1)
        t2 <- hs.get(s2)
        r = (t1, t2) match {
          case (EmptyTrie, EmptyTrie)          => List.empty[(TriePointer, TriePointer)].asLeft[(Trie, Trie)]
          case (l @ EmptyTrie, r @ Skip(_, _)) => (l, r).asRight[List[(TriePointer, TriePointer)]]
          case (l @ EmptyTrie, r @ PointerBlock(_)) =>
            (l, r).asRight[List[(TriePointer, TriePointer)]]
          case (l @ Skip(_, _), r @ EmptyTrie) => (l, r).asRight[List[(TriePointer, TriePointer)]]
          case (l @ Skip(_, ptr), r @ Skip(_, ptr2)) =>
            if (ptr == ptr2) List.empty[(TriePointer, TriePointer)].asLeft[(Trie, Trie)]
            else (l, r).asRight[List[(TriePointer, TriePointer)]]
          case (l @ Skip(_, _), r @ PointerBlock(_)) =>
            (l, r).asRight[List[(TriePointer, TriePointer)]]
          case (l @ PointerBlock(_), r @ EmptyTrie) =>
            (l, r).asRight[List[(TriePointer, TriePointer)]]
          case (l @ PointerBlock(_), r @ Skip(_, _)) =>
            (l, r).asRight[List[(TriePointer, TriePointer)]]
          case (l @ PointerBlock(ptrs), r @ PointerBlock(ptrs2)) =>
            if (ptrs != ptrs2)
              (ptrs zip ptrs2).filter(a => a._1 != a._2).toList.asLeft[(Trie, Trie)]
            else (l, r).asRight[List[(TriePointer, TriePointer)]]
        }
        _ = println(r)
      } yield r

    def pointer(t: TriePointer): Blake2b256Hash = t match {
      case pointer: NonEmptyTriePointer =>
        pointer match {
          case pointer: ValuePointer =>
            pointer match {
              case LeafPointer(hash) => hash
              case NodePointer(hash) => hash
            }
          case SkipPointer(hash) => hash
        }
      case EmptyPointer => Blake2b256Hash.fromHex("")
    }
    def is_(t: TriePointer) = t match {
      case EmptyPointer => true
      case _            => false
    }
    (for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[Task](dataDir, false)
      rspaceStore       <- rnodeStoreManager.rSpaceStores
      hs                = HistoryStoreInstances.historyStore(rspaceStore.history)
      compareTrie <- {
        implicit val h = hs
        (
          List((Blake2b256Hash.fromHex(stateHash1), Blake2b256Hash.fromHex(stateHash2))),
          Result(List.empty, List.empty)
        ).tailRecM[Task, Result] {
          case ((ss1, ss2) :: res, ls) =>
            compare(ss1, ss2).map {
              case Right((l, r)) => Left((res, ls.copy(ls.t ++ List((l, r)))))
              case Left(ns) => {
                val empty = ns.filter(
                  a => is_(a._1) || is_(a._2)
                )
                val nonEmpty = ns.filterNot(
                  a => is_(a._1) || is_(a._2)
                )
                println(nonEmpty)
                println(empty)

                Left(
                  (
                    res ++ nonEmpty.map(a => (pointer(a._1), pointer(a._2))),
                    ls.copy(tp = ls.tp ++ empty)
                  )
                )
              }
            }
          case (Nil, ls) => Task.pure(ls.asRight)
        }
      }
      _ = println(compareTrie)
    } yield ()).runSyncUnsafe()
  }
}
