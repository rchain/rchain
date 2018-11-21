package coop.rchain.rholang.parser

import coop.rchain.rholang.parser.log.impl.LineMapImpl
import org.scalatest.{FlatSpec, Matchers, OneInstancePerTest}

class LineMapImplSpec extends FlatSpec with Matchers with OneInstancePerTest {

  val lineMap = new LineMapImpl("0\n12\r\n345\n\n")

  "LineMapImpl" should "correctly implement 'offsetToRow'" in {

    lineMap.offsetToRow(0) shouldBe 1
    lineMap.offsetToRow(1) shouldBe 1
    lineMap.offsetToRow(2) shouldBe 2
    lineMap.offsetToRow(3) shouldBe 2
    lineMap.offsetToRow(4) shouldBe 2
    lineMap.offsetToRow(5) shouldBe 2
    lineMap.offsetToRow(6) shouldBe 3
    lineMap.offsetToRow(7) shouldBe 3
    lineMap.offsetToRow(8) shouldBe 3
    lineMap.offsetToRow(9) shouldBe 3
    lineMap.offsetToRow(10) shouldBe 4
  }

  "LineMapImpl" should "correctly implement 'offsetToCol'" in {

    lineMap.offsetToCol(0) shouldBe 1
    lineMap.offsetToCol(1) shouldBe 2
    lineMap.offsetToCol(2) shouldBe 1
    lineMap.offsetToCol(3) shouldBe 2
    lineMap.offsetToCol(4) shouldBe 3
    lineMap.offsetToCol(5) shouldBe 4
    lineMap.offsetToCol(6) shouldBe 1
    lineMap.offsetToCol(7) shouldBe 2
    lineMap.offsetToCol(8) shouldBe 3
    lineMap.offsetToCol(9) shouldBe 4
    lineMap.offsetToCol(10) shouldBe 1
  }

  "LineMapImpl" should "correctly implement 'offsetToSrcLine'" in {

    lineMap.offsetToSrcLine(0) shouldBe "0\n"
    lineMap.offsetToSrcLine(1) shouldBe "0\n"
    lineMap.offsetToSrcLine(2) shouldBe "12\r\n"
    lineMap.offsetToSrcLine(3) shouldBe "12\r\n"
    lineMap.offsetToSrcLine(4) shouldBe "12\r\n"
    lineMap.offsetToSrcLine(5) shouldBe "12\r\n"
    lineMap.offsetToSrcLine(6) shouldBe "345\n"
    lineMap.offsetToSrcLine(7) shouldBe "345\n"
    lineMap.offsetToSrcLine(8) shouldBe "345\n"
    lineMap.offsetToSrcLine(9) shouldBe "345\n"
    lineMap.offsetToSrcLine(10) shouldBe "\n"
  }
}
