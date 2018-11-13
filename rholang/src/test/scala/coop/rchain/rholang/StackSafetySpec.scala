package coop.rchain.rholang

import java.io.StringReader

import cats.Parallel
import coop.rchain.models.Connective.ConnectiveInstance.ConnNotBody
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.{Connective, Par, ProtoM}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.StackSafetySpec.findMaxRecursionDepth
import coop.rchain.rholang.interpreter.{Interpreter, PrettyPrinter}
import coop.rchain.rspace.Serialize
import org.scalatest.{Assertions, FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.util.Try
import coop.rchain.catscontrib.effect.implicits._

object StackSafetySpec extends Assertions {
  implicit val parallelTry : Parallel[Try, Try] = Parallel.identity[Try]

  def findMaxRecursionDepth(): Int = {
    def count(i: Int): Int =
      try {
        count(i + 1) //apparently, the try-catch is enough for tailrec to not work. Lucky!
      } catch {
        case _: StackOverflowError => i
      }
    println("About to find max recursion depth for this test run")
    val maxDepth = count(0)
    println(s"Max recursion depth is $maxDepth")
    maxDepth
  }

  //this wrapper allows the test suite to continue with other tests after a SOE is spotted
  def isolateStackOverflow[T](block: => T): T =
    try {
      block
    } catch {
      case e: StackOverflowError => fail("Caused a StackOverflowError", e)
    }

}

class StackSafetySpec extends FlatSpec with Matchers {
  import StackSafetySpec._

  val mapSize: Long               = 10L * 1024L * 1024L
  val tmpPrefix: String           = "rspace-store-"

  val depth: Int = findMaxRecursionDepth()

  //FIXME make all the test cases work with checkAll.
  //To make this happen, we're going to have to change how AST hashCode and serialization work.

  it should "handle a huge string concatenation" in {
    checkAll(hugeBinOp(" ++ ", "\"_\""))
  }

  it should "handle a huge number addition" in {
    checkAll(hugeBinOp(" + ", "1"))
  }

  it should "handle a huge par" in {
    checkAll(hugeBinOp(" | ", "1"))
  }

  it should "handle a huge list" in {
    checkAll("[" + hugeBinOp(", ", "1") + "]")
  }

  it should "handle a huge tuple" in {
    checkAll("(" + hugeBinOp(", ", "1") + ")")
  }

  it should "handle a huge set" in {
    //TODO investigate performance - this is getting very slow for bigger depths
    checkAll("Set(" + binOp(_.toString, ", ") + ")")
  }

  it should "handle a huge map" in {
    //TODO investigate performance - this is getting very slow for bigger depths
    checkAll("{" + binOp(x => s"$x: 1", ", ") + "}")
  }

  it should "handle a huge list concatenation" in {
    checkAll(hugeBinOp(" ++ ", "[1]"))
  }

  it should "handle a huge nested list" in {
    checkAll(hugeNested("[", "", "]"))
  }

  //FIXME java.lang.OutOfMemoryError: GC overhead limit exceeded
  //it should "handle a huge nested set" in {
  //  checkAll(hugeNested("Set(", "42", ")"))
  //}

  //FIXME 11.5 s
  it should "handle a nested new" in {
    checkAll(hugeNested("new x in { ", "1", "}"))
  }

  it should "handle a huge nested name" in {
    checkAll(("*@{" * depth) + "1" + ("}" * depth))
  }

  it should "handle a huge train wreck of method calls" in {
    checkAll("[]" + Seq.fill(depth)(".toByteArray()").mkString)
  }

  it should "handle a huge train wreck of method calls add" in {
    checkAll("Set()" + Seq.fill(depth)(".add(1)").mkString)
  }

  it should "handle a huge nesting of method calls union" in {
    checkAll(hugeNested("Set().union(", "Set()", ")"))
  }

  it should "handle a send with a huge arity" in {
    checkAll("@0!(" + hugeBinOp(", ", "1") + ")")
  }

  it should "handle a huge nested send" in {
    checkAll(hugeNested("@0!(", "1", ")"))
  }

  //FIXME: unbelievably slow: takes 4 s for depth 500 = 8 ms / level
  ignore should "handle a huge receive nested on channel side" in {
    //  for(x <-
    //    @{for(x <- @{for(x <-
    //      @0
    //    ) { Nil }}) { Nil }}
    //  ) { Nil }
    checkAll("for(x <- " + hugeNested("@{for(x <- ", "@0", ") { Nil }}") + ") { Nil }")
  }

  // TODO receive where patterns are other receives
  // TOOO receive with a huge arity
  // TOOO receive with a huge join
  // TODO program that will do n = depth comm events

  private def hugeBinOp(operator: String, operand: String): String =
    Seq.fill(depth)(operand).mkString(operator)

  private def binOp(operand: Int => String, operator: String): String =
    (1 to depth).map(operand).mkString(operator)

  private def hugeNested(left: String, middle: String, right: String): String =
    Seq.fill(depth)(left).mkString + middle + Seq.fill(depth)(right).mkString

  private def checkAll(rho: String): Unit = {
    isolateStackOverflow {
      val ast = Interpreter.buildNormalizedTerm(rho).value()
      PrettyPrinter().buildString(ast)
    }
    checkReduce(rho)
  }

  private def checkReduce(rho: String): Unit =
    isolateStackOverflow {
      //FIXME make this pass:
      //val reduceRho = s"@0!($rho) | for (@x <- @0) { @1 ! (x) | @2 ! (x)  } | for (@y <- @1; @z <- @2) { @0!(Set(y, z)) }"
      //val reduceRho = s"""@0!( { @"serializeMe"!($rho) } )"""
      //val reduceRho = s"""for (_ <- @0) { Nil } | @0!( { @"serializeMe"!($rho) } )"""
      //val reduceRho = s"@0!($rho)"
      val reduceRho = s"for (_ <- @0) { Nil } | @0!($rho)"
      checkSuccess(reduceRho) { rho =>
        mkRuntime[Try, Try](tmpPrefix, mapSize)
          .use { runtime =>
            Interpreter.execute[Try](runtime, new StringReader(rho))
          }
      }
    }

  checkNormalize("Nil") //silence "unused" warnings

  private def checkNormalize(rho: String): Unit =
    isolateStackOverflow {
      checkSuccess(rho) { rho =>
        Try { Interpreter.buildNormalizedTerm(rho)()}
      }
    }

  private def checkSuccess(rho: String)(interpreter: String => Try[_]): Unit =
    interpreter(rho).toEither
      .swap
      .foreach(error => fail(s"""Execution failed for: $rho
                                               |Cause:
                                               |$error""".stripMargin))

}

class SerializationStackSafetySpec extends FlatSpec with Matchers {

  behavior of "Serialize"

  import coop.rchain.models.rholang.implicits._

  val maxRecursionDepth: Int = findMaxRecursionDepth()

  it should "do a round trip of a huge structure" in {

    @tailrec
    def hugePar(n: Int, par: Par = Par(exprs = Seq(GInt(0)))): Par =
      if (n == 0) par
      else hugePar(n - 1, Par(connectives = Seq(Connective(ConnNotBody(par)))))

    val par = hugePar(maxRecursionDepth)

    noException shouldBe thrownBy {
      ProtoM.serializedSize(par).value

      import coop.rchain.models.serialization.implicits._
      val encoded = Serialize[Par].encode(par)
      Serialize[Par].decode(encoded)
    }
  }

}
