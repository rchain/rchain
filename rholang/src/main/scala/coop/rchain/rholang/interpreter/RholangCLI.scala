package coop.rchain.rholang.interpreter

import coop.rchain.shared.StoreType
import java.io.{BufferedOutputStream, FileOutputStream, FileReader, StringReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeoutException

import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.Metrics
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.RhoIStore
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.{Coeval, Task}
import monix.execution.{CancelableFuture, Scheduler}
import org.rogach.scallop.{stringListConverter, ScallopConf}
import coop.rchain.shared.Log

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object RholangCLI {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("Rholang Mercury 0.2")
    banner("""Options:""")

    val binary = opt[Boolean](descr = "outputs binary protobuf serialization")
    val text   = opt[Boolean](descr = "outputs textual protobuf serialization")

    val dataDir = opt[Path](
      required = false,
      descr = "Path to data directory",
      default = Some(Files.createTempDirectory("rspace-store-"))
    )

    val mapSize = opt[Long](
      required = false,
      descr = "Map size (in bytes)",
      default = Some(1024L * 1024L * 1024L)
    )

    val files =
      trailArg[List[String]](required = false, descr = "Rholang source file")(stringListConverter)

    verify()
  }

  def main(args: Array[String]): Unit = {
    import monix.execution.Scheduler.Implicits.global

    val conf = new Conf(args)

    implicit val log: Log[Task]          = Log.log[Task]
    implicit val metricsF: Metrics[Task] = new Metrics.MetricsNOP[Task]()

    val runtime = (for {
      runtime <- Runtime.create[Task, Task.Par](conf.dataDir(), conf.mapSize(), StoreType.LMDB)
      _       <- Runtime.injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
    } yield (runtime)).unsafeRunSync

    try {
      if (conf.files.supplied) {
        val fileNames = conf.files()
        fileNames.foreach(f => processFile(conf, runtime, f))
      } else {
        repl(runtime)
      }
    } finally {
      runtime.close().unsafeRunSync
    }
  }

  def reader(fileName: String): FileReader = new FileReader(fileName)

  private def printPrompt(): Unit =
    Console.print("\nrholang> ")

  private def printNormalizedTerm(normalizedTerm: Par): Unit = {
    Console.println("\nEvaluating:")
    Console.println(PrettyPrinter().buildString(normalizedTerm))
  }

  private def printStorageContents[F[_]](store: RhoIStore[F]): Unit = {
    Console.println("\nStorage Contents:")
    Console.println(StoragePrinter.prettyPrint(store))
  }

  private def printCost(cost: Cost): Unit =
    Console.println(s"Estimated deploy cost: $cost")

  private def printErrors(errors: Vector[Throwable]) =
    if (!errors.isEmpty) {
      Console.println("Errors received during evaluation:")
      for {
        error <- errors
      } Console.println(error.getMessage)
    }

  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Return"))
  def repl(runtime: Runtime[Task])(implicit scheduler: Scheduler): Unit = {
    printPrompt()
    Option(scala.io.StdIn.readLine()) match {
      case Some(line) =>
        Interpreter[Coeval].buildNormalizedTerm(line).runAttempt match {
          case Right(par)                 => evaluatePar(runtime)(par)
          case Left(ie: InterpreterError) =>
            // we don't want to print stack trace for user errors
            Console.err.print(ie.getMessage)
          case Left(th) =>
            th.printStackTrace(Console.err)
        }
      case None =>
        Console.println("\nExiting...")
        return
    }
    repl(runtime)
  }

  def processFile(conf: Conf, runtime: Runtime[Task], fileName: String)(
      implicit scheduler: Scheduler
  ): Unit = {
    val processTerm: Par => Unit =
      if (conf.binary()) writeBinary(fileName)
      else if (conf.text()) writeHumanReadable(fileName)
      else evaluatePar(runtime)

    val source = reader(fileName)

    Interpreter[Coeval]
      .buildNormalizedTerm(source)
      .runAttempt
      .fold(_.printStackTrace(Console.err), processTerm)
  }

  @tailrec
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def waitForSuccess(evaluatorFuture: CancelableFuture[EvaluateResult]): Unit =
    try {
      Await.ready(evaluatorFuture, 5.seconds).value match {
        case Some(Success(EvaluateResult(cost, errors))) =>
          printCost(cost)
          printErrors(errors)
        case Some(Failure(e)) => throw e
        case None             => throw new Exception("Future claimed to be ready, but value was None")
      }
    } catch {
      case _: TimeoutException =>
        Console.println("This is taking a long time. Feel free to ^C and quit.")
        waitForSuccess(evaluatorFuture)
      case e: Exception =>
        throw e
    }

  private def writeHumanReadable(fileName: String)(sortedTerm: Par): Unit = {
    val compiledFileName = fileName.replaceAll(".rho$", "") + ".rhoc"
    new java.io.PrintWriter(compiledFileName) {
      write(sortedTerm.toProtoString)
      close()
    }
    println(s"Compiled $fileName to $compiledFileName")
  }

  private def writeBinary(fileName: String)(sortedTerm: Par): Unit = {
    val binaryFileName = fileName.replaceAll(".rho$", "") + ".bin"
    val output         = new BufferedOutputStream(new FileOutputStream(binaryFileName))
    output.write(sortedTerm.toByteString.toByteArray)
    output.close()
    println(s"Compiled $fileName to $binaryFileName")
  }

  def evaluatePar(runtime: Runtime[Task])(par: Par)(implicit scheduler: Scheduler): Unit = {
    val evaluatorTask =
      for {
        _      <- Task.now(printNormalizedTerm(par))
        result <- Interpreter[Task].evaluate(runtime, par)
      } yield result

    waitForSuccess(evaluatorTask.runToFuture)
    printStorageContents(runtime.space.store)
  }
}
