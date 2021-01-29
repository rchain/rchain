package coop.rchain.rholang

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Connective.ConnectiveInstance.ConnNotBody
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models._
import coop.rchain.models.serialization.implicits._
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.StackSafetySpec.findMaxRecursionDepth
import coop.rchain.rholang.interpreter.{InterpreterUtil, ParBuilderUtil, PrettyPrinter}
import coop.rchain.shared.{Log, Serialize}
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Assertions, FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.concurrent.duration._

object StackSafetySpec extends Assertions {

  val mapSize                             = 10L * 1024L * 1024L
  val tmpPrefix                           = "rspace-store-"
  val maxDuration                         = 20.seconds
  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()

  def findMaxRecursionDepth(): Int = {
    def count(i: Int): Int =
      try {
        count(i + 1) //apparently, the try-catch is enough for tailrec to not work. Lucky!
      } catch {
        case _: StackOverflowError => i
      }

    println("About to find max recursion depth for this test run")

    val maxDepth = count(0)
    println(s"Calculated max recursion depth is $maxDepth")

    // Because of OOM errors on CI depth recursion is limited
    val maxDepthLimited = Math.min(1500, maxDepth)
    println(s"Used recursion depth is limited to $maxDepthLimited")
    maxDepthLimited
  }

  //this wrapper allows the test suite to continue with other tests after a SOE is spotted
  def isolateStackOverflow[T](block: => T): T =
    try {
      block
    } catch {
      case e: StackOverflowError => fail("Caused a StackOverflowError", e)
    }

}

class StackSafetySpec extends FlatSpec with TableDrivenPropertyChecks with Matchers {
  import StackSafetySpec._

  val mapSize     = 1024L * 1024L * 1024L
  val tmpPrefix   = "rspace-store-"
  val maxDuration = 20.seconds
  val depth       = findMaxRecursionDepth()

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
    checkAll(hugeNested("*@{", "1", "}"))
  }

  it should "handle a non-trivial huge nested name" in {
    checkAll(hugeNested("*@(1, ", "*@0", ")"))
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

  private def checkAll(term: String): Unit = {
    implicit val logF: Log[Task] = Log.log[Task]

    val rho =
      s"""
         |  //send without reducing the term, testing serialization
         |  @0!(@"dontReduceYet"!($term)) |
         |
         |  //duplicate the term
         |  for (@x <- @0) { @1 ! (x) | @2 ! (x) } |
         |
         |  //receive both duplicated terms
         |  for (@y <- @1; @z <- @2) {
         |
         |    //create a set to test term's hashCode
         |    @3!(Set(y, z)) |
         |
         |    //compare the terms to test equals
         |    @4!(y == z)
         |  } |
         |
         |  //reduce the term
         |  @5!($term)
         |""".stripMargin

    isolateStackOverflow {
      val ast = ParBuilderUtil.buildNormalizedTerm[Coeval](rho).value()
      PrettyPrinter().buildString(ast)
      checkSuccess(rho) {
        mkRuntime[Task](tmpPrefix).use { runtime =>
          implicit val c = runtime.cost
          InterpreterUtil.evaluateResult(runtime, rho)
        }
      }
    }
  }

  private def checkSuccess(rho: String)(task: => Task[_]): Unit =
    task.attempt
      .runSyncUnsafe(maxDuration)
      .swap
      .foreach(error => fail(s"""Execution failed for: $rho
                                               |Cause:
                                               |$error""".stripMargin))

}

class AstTypeclassesStackSafetySpec extends FlatSpec with Matchers {

  behavior of "AST typeclasses"

  import coop.rchain.models.rholang.implicits._

  val maxRecursionDepth = findMaxRecursionDepth()

  it should "not blow up on a huge structure" in {

    @tailrec
    def hugePar(n: Int, par: Par = Par(exprs = Seq(GInt(0)))): Par =
      if (n == 0) par
      else hugePar(n - 1, Par(connectives = Seq(Connective(ConnNotBody(par)))))

    val par        = hugePar(maxRecursionDepth)
    val anotherPar = hugePar(maxRecursionDepth)

    noException shouldBe thrownBy {
      ProtoM.serializedSize(par).value

      val encoded = Serialize[Par].encode(par)
      Serialize[Par].decode(encoded)

      HashM[Par].hash[Coeval](par).value
      par.hashCode()

      EqualM[Par].equal[Coeval](par, anotherPar).value
      par == anotherPar

    }
  }

}
