package coop.rchain.rholang

import java.io.StringReader
import java.nio.file.Files

import coop.rchain.rholang.interpreter.{Interpreter, PrettyPrinter, Runtime}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class StackSafetySpec extends FlatSpec with Matchers {

  val mapSize     = 10L * 1024L * 1024L
  val tmpPrefix   = "rspace-store-"
  val maxDuration = 5.seconds

  val runtime = Runtime.create(Files.createTempDirectory(tmpPrefix), mapSize)

  val depth: Int = maxRecursionDepth()

  def maxRecursionDepth(): Int = {
    def count(i: Int): Int =
      try {
        count(i + 1) //apparently, the try-catch is enough for tailrec to not work. Lucky!
      } catch {
        case e: StackOverflowError => i
      }
    println("About to find max recursion depth for this test run")
    val maxDepth = count(0)
    println(s"Max recursion depth is $maxDepth")
    maxDepth
  }

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
    checkNormalize(hugeNested("[", "", "]"))
  }

  it should "handle a nested new" in {
    checkNormalize(hugeNested("new x in { ", "1", "}"))
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
    checkNormalize(hugeNested("@0!(", "1", ")"))
  }

  //FIXME: unbelievably slow: takes 4 s for depth 500 = 8 ms / level
  ignore should "handle a huge receive nested on channel side" in {
    //  for(x <-
    //    @{for(x <- @{for(x <-
    //      @0
    //    ) { Nil }}) { Nil }}
    //  ) { Nil }
    checkNormalize("for(x <- " + hugeNested("@{for(x <- ", "@0", ") { Nil }}") + ") { Nil }")
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
      val ast = Interpreter.buildNormalizedTerm(new StringReader(rho)).value()
      PrettyPrinter().buildString(ast)
    }
    checkReduce(rho)
  }

  private def checkReduce(rho: String): Unit =
    isolateStackOverflow {
      val reduceRho = s"for (_ <- @0) { Nil } | @0!($rho)"
      checkSuccess(reduceRho) { rho =>
        Interpreter
          .execute(runtime, new StringReader(rho))
      }
    }

  private def checkNormalize(rho: String): Unit =
    isolateStackOverflow {
      checkSuccess(rho) { rho =>
        Task.coeval(
          Interpreter
            .buildNormalizedTerm(new StringReader(rho))
        )
      }
    }

  //this wrapper allows the test suite to continue with other tests after a SOE is spotted
  def isolateStackOverflow[T](block: => T): T =
    try {
      block
    } catch {
      case e: StackOverflowError => fail("Caused a StackOverflowError", e)
    }

  private def checkSuccess(rho: String)(interpreter: String => Task[_]): Unit =
    interpreter(rho).attempt
      .runSyncUnsafe(maxDuration)
      .swap
      .foreach(error => fail(s"""Execution failed for: $rho
                                               |Cause:
                                               |$error""".stripMargin))

}
