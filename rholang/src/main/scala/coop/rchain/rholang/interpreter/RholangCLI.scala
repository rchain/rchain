package coop.rchain.rholang.interpreter

import java.io.{BufferedOutputStream, FileOutputStream, FileReader, IOException}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeoutException

import cats._
import cats.effect.Sync
import cats.implicits._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoISpace
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.compiler.ParBuilder
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.syntax._
import coop.rchain.shared.Resources
import monix.eval.{Coeval, Task}
import monix.execution.{CancelableFuture, Scheduler}
import org.rogach.scallop.{stringListConverter, ScallopConf}
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.{Failure, Success, Try}

object RholangCLI {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("Rholang Mercury 0.2")
    banner("""Options:""")

    val binary             = opt[Boolean](descr = "outputs binary protobuf serialization")
    val text               = opt[Boolean](descr = "outputs textual protobuf serialization")
    val quiet              = opt[Boolean](descr = "don't print tuplespace after evaluation")
    val unmatchedSendsOnly = opt[Boolean](descr = "only print unmatched sends after evaluation")

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
    implicit val spanF: Span[Task]       = NoopSpan[Task]()
    implicit val parF: Parallel[Task]    = Task.catsParallel
    implicit val kvm                     = InMemoryStoreManager[Task]

    val runtime = (for {
      roots    <- kvm.store("roots")
      cold     <- kvm.store("cold")
      history  <- kvm.store("history")
      channels <- kvm.store("channels")
      space    <- RhoRuntime.setupRhoRSpace[Task](roots, cold, history, channels)
      runtime  <- RhoRuntime.createRhoRuntime[Task](space)
    } yield (runtime)).unsafeRunSync

    val problems = try {
      if (conf.files.supplied) {
        val problems = for {
          f <- conf.files()
          result = processFile(
            conf,
            runtime,
            f,
            conf.quiet.getOrElse(false),
            conf.unmatchedSendsOnly.getOrElse(false)
          ) if result.isFailure
          Failure(problem) = result
        } yield (f, problem)
        problems.foreach {
          case (f, oops) => {
            Console.err.println("error in: " + f)
            errorOrBug(oops) match {
              case Right(err) => Console.err.println(err.getMessage)
              case Left(bug)  => bug.printStackTrace(Console.err)
            }
          }
        }
        problems
      } else {
        repl(runtime)
        List()
      }
    } finally {
      ()
    }
    if (!problems.isEmpty) {
      System.exit(1)
    }
  }

  def errorOrBug(th: Throwable): Either[Throwable, Throwable] = th match {
    // ParserError seems to be about parser construction,
    // i.e. a design-time error that merits a stack trace.
    // LexerError is uses that way too, but it's also
    // used for unexpected EOF and end of string.
    case er: LexerError      => Right(er)
    case er: SyntaxError     => Right(er)
    case er: NormalizerError => Right(er)
    case er: IOException     => Right(er)
    case th: Throwable       => Left(th)
  }

  def reader(fileName: String): FileReader = new FileReader(fileName)

  private def printPrompt(): Unit =
    Console.print("\nrholang> ")

  private def printNormalizedTerm(normalizedTerm: Par): Unit = {
    Console.println("\nEvaluating:")
    Console.println(PrettyPrinter().buildString(normalizedTerm))
  }

  private def printStorageContents[F[_]: Sync](
      runtime: RhoRuntime[F],
      unmatchedSendsOnly: Boolean
  ): F[Unit] =
    Sync[F].delay {
      Console.println("\nStorage Contents:")
    } >> FlatMap[F]
      .ifM(unmatchedSendsOnly.pure[F])(
        ifTrue = StoragePrinter.prettyPrintUnmatchedSends(runtime),
        ifFalse = StoragePrinter.prettyPrint(runtime)
      )
      .map(Console.println)

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
  def repl(runtime: RhoRuntime[Task])(implicit scheduler: Scheduler): Unit = {
    printPrompt()
    Option(scala.io.StdIn.readLine()) match {
      case Some(line) =>
        evaluate(runtime, line).unsafeRunSync
      case None =>
        Console.println("\nExiting...")
        return
    }
    repl(runtime)
  }

  def processFile(
      conf: Conf,
      runtime: RhoRuntime[Task],
      fileName: String,
      quiet: Boolean,
      unmatchedSendsOnly: Boolean
  )(
      implicit scheduler: Scheduler
  ): Try[Unit] = {
    val processTerm: Par => Try[Unit] =
      if (conf.binary()) writeBinary(fileName)
      else if (conf.text()) writeHumanReadable(fileName)
      else
        evaluatePar(
          runtime,
          Resources.withResource(Source.fromFile(fileName))(_.mkString),
          quiet,
          unmatchedSendsOnly
        )

    val source = reader(fileName)

    ParBuilder[Coeval]
      .buildNormalizedTerm(source, Map.empty[String, Par])
      .runAttempt
      .fold(Failure(_), processTerm)

  }

  def evaluate(runtime: RhoRuntime[Task], source: String): Task[Unit] =
    runtime.evaluate(source).map {
      case EvaluateResult(_, Vector()) =>
      case EvaluateResult(_, errors) =>
        errors.foreach {
          case ie: InterpreterError =>
            // we don't want to print stack trace for user errors
            Console.err.print(ie.getMessage)
          case th =>
            th.printStackTrace(Console.err)
        }
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

  private def writeHumanReadable(fileName: String)(sortedTerm: Par): Try[Unit] = {
    val compiledFileName = fileName.replaceAll(".rho$", "") + ".rhoc"
    Try({
      new java.io.PrintWriter(compiledFileName) {
        write(sortedTerm.toProtoString)
        close()
      }
      println(s"Compiled $fileName to $compiledFileName")
    })
  }

  private def writeBinary(fileName: String)(sortedTerm: Par): Try[Unit] = {
    val binaryFileName = fileName.replaceAll(".rho$", "") + ".bin"
    Try({
      val output = new BufferedOutputStream(new FileOutputStream(binaryFileName))
      output.write(sortedTerm.toByteString.toByteArray)
      output.close()
      println(s"Compiled $fileName to $binaryFileName")
    })
  }

  def evaluatePar(
      runtime: RhoRuntime[Task],
      source: String,
      quiet: Boolean,
      unmatchedSendsOnly: Boolean
  )(
      par: Par
  )(implicit scheduler: Scheduler): Try[Unit] = {
    val evaluatorTask =
      for {
        _ <- Task.delay(if (!quiet) {
              printNormalizedTerm(par)
            })
        result <- runtime.evaluate(source)
      } yield result

    Try(waitForSuccess(evaluatorTask.runToFuture)).map { _ok =>
      if (!quiet) {
        printStorageContents(runtime, unmatchedSendsOnly).unsafeRunSync
      }
    }
  }
}
