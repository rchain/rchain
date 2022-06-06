package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.ParBuilderUtil.assertCompiledEqual
import coop.rchain.rholang.interpreter.errors.{SyntaxError, UnexpectedProcContext}
import org.scalatest.EitherValues._
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * This is the first Rholang test class introduced to test a new language construct that
  * does not test the construct on the normalizer level. That is (1) because it is far more
  * efficient to write and test Rholang source than to construct normalized Rholang terms
  * corresponding to the same source, and (2) because this is the first construct that is
  * implemented in terms of a pre-existing Rholang constructs.
  */
class LetSpec extends AnyWordSpec with Matchers {

  "The lexer" should {

    "disallow a non-positive number of let declarations" in {
      ParBuilderUtil.mkTerm("let in { Nil }").left.value shouldBe a[SyntaxError]
    }

    "disallow '&' and ';' from appearing in the same let declaration sequence" in {
      ParBuilderUtil
        .mkTerm("let x <- 1 ; y <- 2 & z <- 3 in { Nil }")
        .left
        .value shouldBe a[SyntaxError]
    }
  }

  "The normalizer" should {

    "treat variables declared in let processes as name variables" when {

      "there is only one declaration" in {
        ParBuilderUtil.mkTerm("let x <- 1 in { x }").left.value shouldBe a[UnexpectedProcContext]
      }

      "there are multiple sequential declarations" in {
        ParBuilderUtil
          .mkTerm("let x <- 1 ; y <- 2 in { x }")
          .left
          .value shouldBe a[UnexpectedProcContext]
      }

      "there are multiple concurrent declarations" in {
        ParBuilderUtil
          .mkTerm("let x <- 1 & y <- 2 in { x }")
          .left
          .value shouldBe a[UnexpectedProcContext]
      }
    }

    "translate a single declaration of multiple variables into a list match process" in {
      // Variables declared in let declarations are treated as name variables,
      // but variables bound by match expressions are treated as process variables.
      // As long as name variables are used in name contexts and process variables are
      // used in process contexts, the following two programs are equivalent because the normalizer
      // eliminates the distinction between names and processes with `Par`, relying on context to
      // decide how to process a given `p: Par`.
      val s = "let x, @y, @{a | b} <- 1, 2, 3 | 4 in { Nil }"
      val t =
        """
          | match [1, 2, 3 | 4] {
          |   [x0, x1, x2 | x3] => { Nil }
          | }""".stripMargin
      assertCompiledEqual(s, t)
    }

    "translate remainders correctly" when {

      "a remainder variable is introduced in a single let declaration" in {
        val s = "let x, y, ... @z <- 1, 2, [3, 4], 5 in { Nil }"
        val t =
          """
            | match [1, 2, [3, 4], 5] {
            |   [x, y ... z] => { Nil }
            | } """.stripMargin
        assertCompiledEqual(s, t)
      }

      "remainder variables are introduced in multiple concurrent let declarations" in {
        val s = "let x, y, ... @z <- 1, 2, [3, 4] & a, b, ... @c <- 5, [6, 7], 8 in { Nil }"
        val t =
          """
            # new x0, x1 in {
            #   x0!(1, 2, [3, 4]) | x1!(5, [6, 7], 8) |
            #   for(x, y, ... @z <- x0  & a, b, ... @c <- x1){ Nil }
            # }""".stripMargin('#')
        assertCompiledEqual(s, t)
      }
    }

    "translate multiple sequential let declarations into nested match processes" in {
      val s = "let x <- 1 ; y <- 2 ; z <- 3 in { Nil }"
      val t =
        """
            | match [1] {
            |   [x0] => {
            |     match [2] {
            |       [x1] => {
            |         match [3] {
            |           [x2] => { Nil }
            |         }
            |       }
            |     }
            |   }
            | }
            | """.stripMargin
      assertCompiledEqual(s, t)
    }

    "translate multiple concurrent let declarations into a COMM" in {
      val s = "let x <- 1 & y <- 2 & z <- 3 in { Nil }"
      val t =
        """
          # new x0, x1, x2 in {
          #   x0!(1) | x1!(2) | x2!(3) |
          #   for(x <- x0  & y <- x1  & z <- x2) { Nil }
          # }""".stripMargin('#')
      assertCompiledEqual(s, t)
    }
  }
}
