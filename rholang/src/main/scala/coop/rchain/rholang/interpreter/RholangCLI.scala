package coop.rchain.rholang.interpreter

import java.io.{BufferedOutputStream, FileOutputStream, FileReader}
import java.nio.file.{Files, Path}

import cats.effect.Resource
import cats.implicits._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.RhoIStore
import coop.rchain.rholang.interpreter.accounting.CostAccount
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.Task
import monix.execution.Scheduler
import org.rogach.scallop.{stringListConverter, ScallopConf, ScallopOption}

import scala.annotation.tailrec
import scala.concurrent.duration._

object RholangCLI {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("Rholang Mercury 0.2")
    banner("""Options:""")

    val binary: ScallopOption[Boolean] =
      opt[Boolean](descr = "outputs binary protobuf serialization")
    val text: ScallopOption[Boolean] =
      opt[Boolean](descr = "outputs textual protobuf serialization")

    val dataDir: ScallopOption[Path] = opt[Path](
      required = false,
      descr = "Path to data directory",
      default = Some(Files.createTempDirectory("rspace-store-"))
    )

    val mapSize: ScallopOption[Long] = opt[Long](
      required = false,
      descr = "Map size (in bytes)",
      default = Some(1024L * 1024L * 1024L)
    )

    val files: ScallopOption[List[String]] =
      trailArg[List[String]](required = false, descr = "Rholang source file")(stringListConverter)

    verify()
  }

  def main(args: Array[String]): Unit = {
    import cats.implicits._
    import monix.execution.Scheduler.Implicits.global

    val conf = new Conf(args)

    val runtimeResource: Resource[Task, Runtime] =
      Resource
        .make(Runtime.create(conf.dataDir(), conf.mapSize()))(_.close())
        .flatMap(
          runtime =>
            Resource.liftF(
              Runtime
                .injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
                .map(Function.const(runtime))
            )
        )

    if (conf.files.supplied) {
      runtimeResource.use { runtime =>
        conf
          .files()
          .traverse[Task, Unit](f => processFile(conf, runtime, f))
          .map(_.combineAll)
      }
    } else {
      runtimeResource.use(runtime => Task.delay(repl(runtime)))
    }.runSyncUnsafe(Duration.Inf)
  }

  def reader(fileName: String): FileReader = new FileReader(fileName)

  private def printPrompt(): Unit =
    Console.print("\nrholang> ")

  private def printNormalizedTerm(normalizedTerm: Par): Unit = {
    Console.println("\nEvaluating:")
    Console.println(PrettyPrinter().buildString(normalizedTerm))
  }

  private def printStorageContents(store: RhoIStore): Unit = {
    Console.println("\nStorage Contents:")
    Console.println(StoragePrinter.prettyPrint(store))
  }

  private def printCost(cost: CostAccount): Unit =
    Console.println(s"Estimated deploy cost: $cost")

  def processFile(conf: Conf, runtime: Runtime, fileName: String): Task[Unit] = {
    val processTerm: Par => Task[Unit] =
      if (conf.binary()) writeBinary(fileName)
      else if (conf.text()) writeHumanReadable(fileName)
      else evaluatePar(runtime)

    val source = reader(fileName)

    Interpreter
      .buildNormalizedTerm(source)
      .runAttempt
      .traverse(processTerm)
      .map(result => result.leftMap(ex => ex.printStackTrace(Console.err)))
  }

  private def printErrors(errors: Vector[Throwable]): Unit =
    if (errors.nonEmpty) {
      Console.println("Errors received during evaluation:")
      for {
        error <- errors
      } Console.println(error.getMessage)
    }

  @tailrec
  def repl(runtime: Runtime)(implicit scheduler: Scheduler): Unit = {
    printPrompt()
    Option(scala.io.StdIn.readLine()) match {
      case Some(line) =>
        Interpreter.buildNormalizedTerm(line).runAttempt match {
          case Right(par)                 => evaluatePar(runtime)(par)
          case Left(ie: InterpreterError) =>
            // we don't want to print stack trace for user errors
            Console.err.print(ie.getMessage)
          case Left(th) =>
            th.printStackTrace(Console.err)
        }
      case None =>
        Console.println("\nExiting...")
    }
    repl(runtime)
  }

  def doAfter[A](timeout: FiniteDuration, action: => Unit)(task: Task[A]): Task[A] = {
    val longTaskHelper =
      for {
        _ <- Task.sleep(timeout)
        _ <- Task.delay(action)
        _ <- Task.never[Unit]
      } yield ()

    Task
      .race(task, longTaskHelper)
      .map(_.left.get)
  }

  private def writeHumanReadable(fileName: String)(sortedTerm: Par): Task[Unit] = Task.delay {
    val compiledFileName = fileName.replaceAll(".rho$", "") + ".rhoc"
    new java.io.PrintWriter(compiledFileName) {
      write(sortedTerm.toProtoString)
      close()
    }
    println(s"Compiled $fileName to $compiledFileName")
  }

  private def writeBinary(fileName: String)(sortedTerm: Par): Task[Unit] = Task.delay {
    val binaryFileName = fileName.replaceAll(".rho$", "") + ".bin"
    val output         = new BufferedOutputStream(new FileOutputStream(binaryFileName))
    output.write(sortedTerm.toByteString.toByteArray)
    output.close()
    println(s"Compiled $fileName to $binaryFileName")
  }

  def evaluatePar(runtime: Runtime)(par: Par): Task[Unit] = {
    val evaluation: Task[Unit] =
      for {
        _      <- Task.now(printNormalizedTerm(par))
        result <- Interpreter.evaluate(runtime, par)
      } yield {
        printStorageContents(runtime.space.store)
        printCost(result.cost)
        printErrors(result.errors)
      }

    doAfter(5.seconds, Console.println("This is taking a long time. Feel free to ^C and quit."))(
      evaluation
    )
  }
}
