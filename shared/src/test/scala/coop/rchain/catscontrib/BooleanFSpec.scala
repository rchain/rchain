package coop.rchain.catscontrib

import cats.{Id, Monad}
import cats.syntax.all._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BooleanFSpec extends AnyFunSpec with Matchers with ToBooleanF {

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

  describe("evaluation of boolean combinators arguments") {
    import BooleanF._

    def traceExecution[F[_]: Monad](
        aInit: Boolean,
        bInit: Boolean,
        f: (=> F[Boolean], => F[Boolean]) => F[Boolean]
    ) = {
      var aCounter = 0
      var bCounter = 0

      def a = {
        aCounter = aCounter + 1
        aInit.pure[F]
      }

      def b = {
        bCounter = bCounter + 1
        bInit.pure[F]
      }

      f(a, b) >> (aCounter, bCounter).pure[F]
    }

    it("AND and OR should be lazy to evaluate second argument") {
      traceExecution[Id](false, true, (x, y) => x &&^ y) shouldBe (1, 0)

      traceExecution[Id](true, true, (x, y) => x ||^ y) shouldBe (1, 0)
    }

    it("NOT should be eager to evaluate argument") {
      traceExecution[Id](true, true, (x, y) => {
        val _ = (~^(x), y.not)
        true.pure[Id]
      }) shouldBe (1, 1)
    }
  }

}
