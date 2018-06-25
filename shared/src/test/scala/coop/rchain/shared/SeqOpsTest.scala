package coop.rchain.shared

import org.scalatest.{FlatSpec, Matchers}
import scala.collection.immutable.Seq
import coop.rchain.shared.SeqOps._

class SeqOpsTest extends FlatSpec with Matchers {

  "dropIndex" should "remove first element" in {
    dropIndex(Seq(1, 2, 3), 0) shouldBe Seq(2, 3)
  }

  "dropIndex" should "remove single element" in {
    dropIndex(Seq(1), 0) shouldBe Seq.empty[Int]
  }

  "dropIndex " should "remove last element" in {
    dropIndex(Seq(1, 2, 3), 2) shouldBe Seq(1, 2)
  }

  "dropIndex " should "remove element in the middle of sequence" in {
    dropIndex(Seq(1, 2, 3), 1) shouldBe Seq(1, 3)
  }

  "dropIndex" should "ignore invalid indexes" in {
    an[IndexOutOfBoundsException] shouldBe thrownBy(dropIndex(Seq(1, 2, 3), 10))
  }

  "removeFirst" should "remove first element" in {
    removeFirst(Seq(1, 2, 3))(_ => false) shouldBe Seq(1, 2, 3)
  }

  "removeFirst" should "remove single element" in {
    removeFirst(Seq(1))(_ => true) shouldBe Seq.empty[Int]
  }

  "removeFirst" should "remove first matched element" in {
    removeFirst(Seq(1, 2, 3))(_ => true) shouldBe Seq(2, 3)
  }

  "removeFirst" should "remove only the first element that matches the predicate" in {
    removeFirst(Seq(1, 2, 3, 2))(_ == 2) shouldBe Seq(1, 3, 2)
  }

  "removeFirst" should "work on empty collections" in {
    removeFirst(Seq.empty[Int])(_ => true) shouldBe Seq.empty[Int]
    removeFirst(Seq.empty[Int])(_ => false) shouldBe Seq.empty[Int]
  }
}
