package coop.rchain.rholang

import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.{Expr, Par}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{EvaluateResult, RhoRuntime}
import coop.rchain.rholang.syntax._
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import java.io.File
import scala.concurrent.duration._
import scala.io.Source

class Rholang11Spec extends FlatSpec with Matchers {
  private val tmpPrefix   = "rspace-store-"
  private val maxDuration = 5.seconds

  implicit val logF: Log[Task]            = Log.log[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()

  def getListOfRholangContract(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(f => f.isFile && f.getName.endsWith(".rho")).toList
    } else {
      List[File]()
    }
  }

  val files = getListOfRholangContract("rholang/examples/rholang1.1")

  files.foreach(f => {
    s"rholang example ${f.getName}" should "execute with no errors" in {
      val file    = Source.fromFile(f)
      val content = file.getLines().mkString("\n")
      println(s"executing ${f.getName}")
      mkRuntime[Task](tmpPrefix)
        .use { runtime =>
          runtime
            .evaluate(content)
            .map(
              res =>
                assert(
                  res.errors.isEmpty,
                  s"""Execution failed for: $content
                       |Cause:
                       |${res.errors}""".stripMargin
                )
            )
        }
        .runSyncUnsafe(maxDuration)
      file.close()
    }
  })

// below codes are commented out and left
// because it could help developer to debug the rholang in rholang1.1 folder just
// by uncommented the codes below and run it with the file name

//  s"rholang example" should "execute with no errors" in {
//    val name = "tut-registry.rho"
////    val file = Source.fromFile(s"rholang/examples/${name}")
//    val file    = Source.fromFile(s"rholang/examples/rholang1.1/${name}")
//    val content = file.getLines().mkString("\n")
//    mkRuntime[Task](tmpPrefix)
//      .use { runtime =>
//        runtime
//          .evaluate(content)
//          .map(
//            res =>
//              assert(
//                res.errors.isEmpty,
//                s"""Execution failed for: $content
//                   |Cause:
//                   |${res.errors}""".stripMargin
//              )
//          )
//      }
//      .runSyncUnsafe()
//    file.close()
//  }
}
