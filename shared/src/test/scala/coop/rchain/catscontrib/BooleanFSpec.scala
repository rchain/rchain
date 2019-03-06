package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import org.scalatest._

class BooleanFSpec extends FunSpec with Matchers with ToBooleanF {

  describe("boolean OR") {
    it("FALSE || FALSE = FALSE") {
      val b1: Id[Boolean] = false
      val b2: Id[Boolean] = false
      b1.||^(b2) shouldBe (false)
    }

    it("FALSE || TRUE = TRUE") {
      val b1: Id[Boolean] = false
      val b2: Id[Boolean] = true
      b1.||^(b2) shouldBe (true)
    }

    it("TRUE || FALSE = TRUE") {
      val b1: Id[Boolean] = true
      val b2: Id[Boolean] = false
      b1.||^(b2) shouldBe (true)
    }

    it("TRUE || TRUE = TRUE") {
      val b1: Id[Boolean] = true
      val b2: Id[Boolean] = true
      b1.||^(b2) shouldBe (true)
    }
  }

  describe("boolean NOT") {

    import BooleanF._

    it("~TRUE=FALSE & ~FALSE=TRUE") {
      val b1: Id[Boolean] = false
      val b2: Id[Boolean] = true
      ~^(b1) shouldBe (b2)
      ~^(b2) shouldBe (b1)
    }

  }

}
