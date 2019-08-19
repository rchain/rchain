package coop.rchain.rspace.history

import monix.eval.Task
import org.scalatest.{Assertion, FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import TestData._
import coop.rchain.rspace.Blake2b256Hash
import History.KeyPath
import scodec.bits.ByteVector
import cats.implicits._

import scala.util.Random

class HistorySpec extends FlatSpec with Matchers with OptionValues with InMemoryHistoryTestBase {
  "first leaf in trie" should "create a Skip -> Leaf" in withEmptyTrie { emptyHistory =>
    val data = insert(_zeros) :: Nil
    for {
      newHistory <- emptyHistory.process(data)
      result     <- newHistory.find(_zeros)
      (_, path)  = result
      _ = path match {
        case skip =>
          skip shouldBe Vector(Skip(_zeros, LeafPointer(data.head.hash)))
      }
    } yield ()
  }

  "leafs under same prefix" should "live in one pointer block" in withEmptyTrie { emptyHistory =>
    val data = List.range(0, 10).map(zerosAnd).map(k => InsertAction(k, randomBlake))
    for {
      newHistory          <- emptyHistory.process(data)
      results             <- data.traverse(action => newHistory.find(action.key))
      _                   = results should have size 10
      (_, headPath)       = results.head
      secondTrieOnPath    = headPath.tail.head
      _                   = secondTrieOnPath shouldBe a[PointerBlock]
      allFirstTriesOnPath = results.map(_._2.head).toSet
      _                   = allFirstTriesOnPath should have size 1
      firstSkip           = allFirstTriesOnPath.head
      _                   = skipShouldHaveAffix(firstSkip, _31zeros)
    } yield ()
  }

  "deletion of a leaf" should "result in empty store" in withEmptyTrie { emptyHistory =>
    val insertions = insert(_zeros) :: Nil
    val deletions  = delete(_zeros) :: Nil
    for {
      historyOne  <- emptyHistory.process(insertions)
      historyTwo  <- historyOne.process(deletions)
      result      <- historyTwo.find(_zeros)
      (ptr, path) = result
      _           = path should have size 0
      _           = ptr shouldBe a[EmptyPointer.type]
    } yield ()
  }

  "deletion of a leaf" should "collapse two skips" in withEmptyTrie { emptyHistory =>
    val inserts   = insert(_zeros) :: insert(_zerosOnes) :: Nil
    val deletions = delete(_zeros) :: Nil
    for {
      historyOne  <- emptyHistory.process(inserts)
      historyTwo  <- historyOne.process(deletions)
      result      <- historyTwo.find(_zerosOnes)
      (ptr, path) = result
      _           = path should have size 1
      _           = path.head shouldBe a[Skip]
      _           = skipShouldHaveAffix(path.head, _zerosOnes)
      _           = ptr shouldBe a[LeafPointer]
    } yield ()
  }

  "update of a leaf" should "not change past history" in withEmptyTrie { emptyHistory =>
    val insertOne = insert(_zeros) :: Nil
    val insertTwo = insert(_zeros) :: Nil
    for {
      historyOne       <- emptyHistory.process(insertOne)
      resultOnePre     <- historyOne.find(_zeros)
      historyTwo       <- historyOne.process(insertTwo)
      resultOnePost    <- historyOne.find(_zeros)
      resultTwo        <- historyTwo.find(_zeros)
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
      HistoryInstances.merging[Task](History.emptyRootHash, inMemHistoryStore)
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
  def prefixWithZeros(s: String): KeyPath = {
    val a = List.fill(32 - (s.length))(0).map(_.toByte)
    val b = s.toCharArray.toList.map(_.asDigit).map(_.toByte)
    a ++ b
  }

  def randomBlake: Blake2b256Hash =
    Blake2b256Hash.create(Random.alphanumeric.take(32).map(_.toByte).toArray)

  def zerosBlake: Blake2b256Hash =
    Blake2b256Hash.create(List.fill(32)(0).map(_.toByte).toArray)

  def insert(k: KeyPath) =
    InsertAction(k, randomBlake)

  def delete(k: KeyPath) =
    DeleteAction(k)
}
