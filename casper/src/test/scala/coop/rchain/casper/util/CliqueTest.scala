package coop.rchain.casper.util

import coop.rchain.casper.helper.{BlockGenerator, BlockStoreFixture}
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.annotation.tailrec

class CliqueTest extends FlatSpec with Matchers with BlockGenerator with BlockStoreFixture {
  val e = List(
    (1, 6),
    (1, 2),
    (1, 3),
    (2, 6),
    (2, 4),
    (2, 3),
    (3, 6),
    (4, 6),
    (4, 7),
    (4, 5),
    (5, 7),
    (8, 9),
    (10, 11)
  )

  private def compare(l: List[Int], r: List[Int]): Boolean = {
    @tailrec
    def compareHelper(l: List[Int], r: List[Int]): Boolean = (l, r) match {
      case (lH :: lT, rH :: rT) if lH == rH => compareHelper(lT, rT)
      case (lH :: _, rH :: _) if lH < rH    => true
      case _                                => false
    }

    compareHelper(l, r)
  }

  private def assertCliquesEqual(src: List[List[Int]], expected: List[List[Int]]): Assertion =
    src.map(_.sorted).sortWith(compare) should equal(expected.map(_.sorted).sortWith(compare))

  "findCliquesRecursive" should "yield all cliques" in {
    val c        = Clique.findCliquesRecursive(e)
    val expected = List(List(2, 6, 1, 3), List(2, 6, 4), List(5, 4, 7), List(8, 9), List(10, 11))
    assertCliquesEqual(c.toList, expected)
  }

  "findCliquesRecursive" should "work well in self loops" in {
    val g        = (1, 1) :: e
    val c        = Clique.findCliquesRecursive(g)
    val expected = List(List(2, 6, 1, 3), List(2, 6, 4), List(5, 4, 7), List(8, 9), List(10, 11))
    assertCliquesEqual(c.toList, expected)
  }

  "findCliquesRecursive" should "work well in another case" in {
    val H = List((1, 2), (1, 4), (1, 5), (1, 6), (2, 3), (3, 4), (3, 6), (4, 5), (4, 6), (5, 6))

    val c        = Clique.findCliquesRecursive(H)
    val expected = List(List(1, 2), List(1, 4, 5, 6), List(2, 3), List(3, 4, 6))

    assertCliquesEqual(c.toList, expected)
  }

  "findCliquesRecursive" should "work well when there is no cliques" in {
    val H = List()

    val c        = Clique.findCliquesRecursive(H)
    val expected = List()

    assertCliquesEqual(c.toList, expected)
  }
}
