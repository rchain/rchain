package coop.rchain.rosette

import coop.rchain.rosette.utils.pSlice
import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable

class UtilsSpec extends WordSpec with Matchers {
  val seq = mutable.Seq(1, 2, 3, 4, 5)
  val newValue = 1337
  val start = 1
  val end = 2

  "A pSlice" should {

    "return correct size" in {
      pSlice(seq, start, end).length shouldEqual (end - start)
    }

    "return max size when end is out of bounds" in {
      val outbound = seq.size + 3
      val end = seq.size
      pSlice(seq, start, outbound).length shouldEqual (end - start)
    }

    "mutate on origin seq" in {
      val seq = mutable.Seq(1, 2, 3, 4)
      val slice = pSlice(seq, 0, seq.size)
      slice.update(0, newValue)
      seq(0) shouldEqual newValue
    }

    "mutate on origin seq based on start index" in {
      val seq = mutable.Seq(1, 2, 3, 4)
      val slice = pSlice(seq, start, seq.size)
      slice.update(0, newValue)
      seq(start) shouldEqual newValue
    }

    "mutate on slice" in {
      val seq = mutable.Seq(1, 2, 3, 4)
      val slice = pSlice(seq, 0, seq.size)
      slice.update(0, newValue)
      slice(0) shouldEqual newValue
    }

    "mutate on slice when seq updated" in {
      val seq = mutable.Seq(1, 2, 3, 4)
      val slice = pSlice(seq, 0, seq.size)
      seq.update(0, newValue)
      slice(0) shouldEqual newValue
    }

    "should return element from origin seq based on start index" in {
      val slice = pSlice(seq, start, end)
      slice(0) shouldEqual seq(start)
    }
  }
}
