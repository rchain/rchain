package coop.rchain.rspace.nextgenrspace.history

import monix.eval.Task
import org.scalatest.{Assertion, FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import TestData._
import coop.rchain.rspace.Blake2b256Hash
import History.{codecTrie, KeyPath, Trie}
import scodec.bits.ByteVector
import cats.implicits._

import scala.util.Random

class HistorySpec extends FlatSpec with Matchers with OptionValues with InMemoryHistoryTestBase {
  "first leaf in trie" should "create a Skip -> Leaf" in withEmptyTrie { emptyHistory =>
    val data = insert(_zeros) :: Nil
    for {
      newHistory <- emptyHistory.process(data)
      result     <- newHistory.findPath(_zeros)
      (_, path)  = result
      _ = path.nodes match {
        case skip +: leaf =>
          leaf shouldBe Vector(Leaf(data.head.hash))
          skip shouldBe Skip(_zeros, Trie.hash(leaf.head))
      }
    } yield ()
  }

  "leafs under same prefix" should "live in one pointer block" in withEmptyTrie { emptyHistory =>
    val data = List.range(0, 10).map(zerosAnd).map(k => InsertAction(k, randomBlake))
    for {
      newHistory          <- emptyHistory.process(data)
      results             <- data.traverse(action => newHistory.findPath(action.key))
      _                   = results should have size 10
      (_, headPath)       = results.head
      secondTrieOnPath    = headPath.nodes.tail.head
      _                   = secondTrieOnPath shouldBe a[Node]
      allFirstTriesOnPath = results.map(_._2.nodes.head).toSet
      _                   = allFirstTriesOnPath should have size 1
      firstSkip           = allFirstTriesOnPath.head
      _                   = skipShouldHaveAffix(firstSkip, _31zeros)
    } yield ()
  }

  "deletion of a leaf" should "collapse two skips" in withEmptyTrie { emptyHistory =>
    val inserts   = insert(_zeros) :: insert(_zerosOnes) :: Nil
    val deletions = delete(_zeros) :: Nil
    for {
      historyOne <- emptyHistory.process(inserts)
      historyTwo <- historyOne.process(deletions)
      result     <- historyTwo.findPath(_zerosOnes)
      (_, path)  = result
      _          = path.nodes should have size 2
      _          = path.nodes.head shouldBe a[Skip]
      _          = path.nodes.last shouldBe a[Leaf]
      _          = skipShouldHaveAffix(path.nodes.head, _zerosOnes)
    } yield ()
  }

  "update of a leaf" should "not change past history" in withEmptyTrie { emptyHistory =>
    val insertOne = insert(_zeros) :: Nil
    val insertTwo = insert(_zeros) :: Nil
    for {
      historyOne       <- emptyHistory.process(insertOne)
      resultOnePre     <- historyOne.findPath(_zeros)
      historyTwo       <- historyOne.process(insertTwo)
      resultOnePost    <- historyOne.findPath(_zeros)
      resultTwo        <- historyTwo.findPath(_zeros)
      (leafOnePre, _)  = resultOnePre
      (leafOnePost, _) = resultOnePost
      (leafTwo, _)     = resultTwo
      _                = leafOnePre shouldBe leafOnePre
      _                = leafOnePre should not be leafTwo
      _                = leafOnePost should not be leafTwo
    } yield ()
  }

  protected def withEmptyTrie(f: History[Task] => Task[Unit]): Unit = {
    val emptyHistory =
      new History[Task](emptyRootHash, inMemHistoryStore, inMemPointerBlockStore)
    f(emptyHistory).runSyncUnsafe(20.seconds)
  }

  def skipShouldHaveAffix(t: Trie, bytes: KeyPath): Assertion =
    t match {
      case Skip(affix, _) => affix.toSeq.toList shouldBe bytes
      case p              => fail("unknown trie prefix" + p)
    }
}

object TestData {

  implicit def toByteVector(bytes: KeyPath): ByteVector = ByteVector(bytes)

  val _zeros: KeyPath           = List.fill(32)(0).map(_.toByte)
  val _zerosOnes: KeyPath       = (List.fill(16)(0) ++ List.fill(16)(1)).map(_.toByte)
  val _31zeros: KeyPath         = List.fill(31)(0).map(_.toByte)
  def zerosAnd(i: Int): KeyPath = _31zeros :+ i.toByte

  def randomBlake: Blake2b256Hash =
    Blake2b256Hash.create(Random.alphanumeric.take(32).map(_.toByte).toArray)

  def insert(k: KeyPath) =
    InsertAction(k, randomBlake)

  def delete(k: KeyPath) =
    DeleteAction(k)
}
