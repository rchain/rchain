package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.test.TestKey32
import monix.eval.Task
import org.scalatest.{Assertion, FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import TestData._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.nextgenrspace.history.History.Trie
import scodec.bits.ByteVector
import History.codecTrie
import cats.implicits._

import scala.util.Random

class HistorySpec extends FlatSpec with Matchers with OptionValues with InMemoryHistoryTestBase {
  "first leaf in trie" should "create a Skip -> Leaf" in withEmptyTrie { emptyHistory =>
    val data = insert(_zeros) :: Nil
    for {
      newHistory <- emptyHistory.process(data)
      result     <- newHistory.findPath(_zeros)
      (_, path)  = result
      _ = path.partialPath match {
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
      results             <- data.traverse(action => newHistory.findPath(action.key.bytes.toSeq.toList))
      _                   = results should have size 10
      (_, headPath)       = results.head
      secondTrieOnPath    = headPath.partialPath.tail.head
      _                   = secondTrieOnPath shouldBe a[Node]
      allFirstTriesOnPath = results.map(_._2.partialPath.head).toSet
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
      _          = path.partialPath should have size 2
      _          = path.partialPath.head shouldBe a[Skip]
      _          = path.partialPath.last shouldBe a[Leaf]
      _          = skipShouldHaveAffix(path.partialPath.head, _zerosOnes)
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

  protected def withEmptyTrie(f: History[Task, TestKey32] => Task[Unit]): Unit = {
    val emptyHistory =
      new History[Task, TestKey32](emptyRootHash, inMemHistoryStore, inMemPointerBlockStore)
    f(emptyHistory).runSyncUnsafe(20.seconds)
  }

  def skipShouldHaveAffix(t: Trie, bytes: List[Byte]): Assertion =
    t match {
      case Skip(affix, _) => affix.toSeq.toList shouldBe bytes
      case p              => fail("unknown trie prefix" + p)
    }
}

object TestData {

  implicit def convertIntListToByteList(l: List[Int]): List[Byte] = l.map(_.toByte)
  implicit def convertIntListToByteArray(l: List[Int]): Array[Byte] =
    convertIntListToByteList(l).toArray
  implicit def convertIntListToByteVector(l: List[Int]): ByteVector =
    ByteVector(convertIntListToByteArray(l))

  val _zeros: List[Int]           = List.fill(32)(0)
  val _zerosOnes: List[Int]       = List.fill(16)(0) ++ List.fill(16)(1)
  val _31zeros: List[Int]         = List.fill(31)(0)
  def zerosAnd(i: Int): TestKey32 = TestKey32.create(_31zeros :+ i)

  def randomBlake: Blake2b256Hash =
    Blake2b256Hash.create(Random.alphanumeric.take(32).map(_.toByte).toArray)

  def insert(k: List[Int]) =
    InsertAction(TestKey32.create(k), randomBlake)

  def delete(k: List[Int]) =
    DeleteAction(TestKey32.create(k))
}
