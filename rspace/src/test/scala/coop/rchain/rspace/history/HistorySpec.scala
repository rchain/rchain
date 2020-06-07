package coop.rchain.rspace.history

import monix.eval.Task
import org.scalatest.{Assertion, FlatSpec, Matchers, OptionValues}
import org.scalacheck.{Gen}
import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import TestData._
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.crypto.codec.Base16
import History.KeyPath
import scodec.bits.ByteVector
import cats.implicits._

import scala.util.Random

class HistorySpec extends FlatSpec with Matchers with OptionValues with InMemoryHistoryTestBase {

  // Empty                     root
  //                            |
  //          insert           Skip
  //        ============>       |
  //                           Leaf
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

  // EmptyTrie                                                   root
  //                                                              |
  //                                                            Skip
  //                    insert with same prefix                   |
  //                  ========================>              PointerBlock
  //                                                            / | \
  //                                                           /  |  \
  //                                                       Leaf  Leaf  Leaf
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

  //          root                          root ---> EmptyTrie
  //           |
  //          Skip
  //           |        Delete(Leaf)
  //          Leaf    ===============>
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

  //          root                                                 root
  //           |                                                    |
  //          Skip                                                 Skip
  //           |                                                    |
  //       PointerBlock                                            Leaf
  //        /       \
  //     Skip       Skip
  //       |          |       delete one of the leaf
  //     Leaf       Leaf     =========================>
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

  //        first                                      historyOne                                   historyTwo
  //                         insert key1 with value1                  insert key1 with value2
  //   root-> EmptyTrie    ===========================>  root    ===============================>   root
  //                                                      |                                           |
  //                                                     Skip                                       Skip
  //                                                      |                                           |
  //                                                     Leaf(value1)                               Leaf(value2)
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

  //              root                                                                      root
  //               |                                                                          |
  //              Skip                                                                      Skip
  //               |               delete Leaf1                                               |
  //           PointerBlock       then insert one alone with Leaf3                      PointerBlock
  //           /       \         ==================================>                    /         \
  //        Skip       Skip                                                           Skip       Skip
  //           |          |                                                             |          |
  //        Leaf1       Leaf2                                                         Leaf3       Leaf2

  "delete collapsed a branch then insert" should "work fine" in withEmptyTrie { emptyHistory =>
    val inserts =
      insert(hexKey("000100")) ::
        insert(hexKey("000220")) ::
        Nil
    val insertsTwo =
      delete(hexKey("000100")) ::
        insert(hexKey("000110")) ::
        Nil

    for {
      historyOne                    <- emptyHistory.process(inserts)
      keyOne                        <- historyOne.find(inserts(0).key)
      keyTwo                        <- historyOne.find(inserts(1).key)
      historyTwo                    <- historyOne.process(insertsTwo)
      keyOneNotExist                <- historyTwo.find(insertsTwo(0).key)
      newInsert                     <- historyTwo.find(insertsTwo(1).key)
      keyTwoAfterProcess            <- historyTwo.find(inserts(1).key)
      (_, p1)                       = keyOne
      (keyTwoValue, p2)             = keyTwo
      (keyTwoAfterProcessValue, p3) = keyTwoAfterProcess
      (_, p4)                       = newInsert
      (keyOneNotExistEmpty, _)      = keyOneNotExist
      _                             = keyTwoAfterProcessValue shouldBe keyTwoValue
      _                             = keyOneNotExistEmpty shouldBe a[EmptyPointer.type]
      _                             = skipShouldHaveAffix(p1.head, List(0, 0))
      _                             = skipShouldHaveAffix(p2.head, List(0, 0))
      _                             = skipShouldHaveAffix(p3.head, List(0, 0))
      _                             = skipShouldHaveAffix(p1.last, List(0, 0))
      _                             = skipShouldHaveAffix(p2.last, List(2, 0))
      _                             = skipShouldHaveAffix(p3.last, List(2, 0))
      _                             = skipShouldHaveAffix(p4.head, List(0, 0))
      _                             = skipShouldHaveAffix(p4.last, List(1, 0))
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

  def hexKey(s: String) = Base16.unsafeDecode(s).toList

  def randomBlake: Blake2b256Hash =
    Blake2b256Hash.create(Random.alphanumeric.take(32).map(_.toByte).toArray)

  def zerosBlake: Blake2b256Hash =
    Blake2b256Hash.create(List.fill(32)(0).map(_.toByte).toArray)

  def insert(k: KeyPath) =
    InsertAction(k, randomBlake)

  def delete(k: KeyPath) =
    DeleteAction(k)
}
