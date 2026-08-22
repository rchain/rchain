package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.ParBuilderUtil.assertCompiledEqual
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SequentialConsumeSpec extends AnyWordSpec with Matchers {

  "The normalizer" should {

    "translate one sequential join into multiple nested consumes" in {

      val s = "for(x0 <- @0 & x1 <- @1 & x2 <- @2 ; x3 <- @3 & x4 <- @4 & x5 <- @5){ Nil }"
      val t =
        """
            # for(x0 <- @0 & x1 <- @1 & x2 <- @2){
            #   for(x3 <- @3 & x4 <- @4 & x5 <- @5){
            #     Nil
            #   }
            # }
            # """.stripMargin('#')
      assertCompiledEqual(s, t)
    }
  }
}
