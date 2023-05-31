package coop.rchain.rholang.interpreter

import cats._
import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Sync}
import cats.syntax.all._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.compiler.Compiler
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.syntax._
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Log
import coop.rchain.store.LmdbDirStoreManager.{mb, Db, LmdbEnvConfig}
import coop.rchain.store.{KeyValueStoreManager, LmdbDirStoreManager}
import org.rogach.scallop.{stringListConverter, ScallopConf}

import java.io.{BufferedOutputStream, FileOutputStream, FileReader, IOException}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeoutException
import scala.annotation.tailrec
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

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

    val conf = new Conf(args.toIndexedSeq)

    implicit val log: Log[IO]          = Log.log[IO]
    implicit val metricsF: Metrics[IO] = new Metrics.MetricsNOP[IO]()
    implicit val spanF: Span[IO]       = NoopSpan[IO]()

    val kvm = mkRSpaceStoreManager[IO](conf.dataDir(), conf.mapSize()).unsafeRunSync()

    val runtime = (for {
      store   <- kvm.rSpaceStores
      runtime <- RhoRuntime.createRuntime[IO](store, Par())
    } yield runtime).unsafeRunSync()

    val problems = try {
      if (conf.files.isSupplied) {
        val problems = for {
          f <- conf.files()
          result = processFile(
            conf,
            runtime,
            f,
            conf.quiet.getOrElse(false),
            conf.unmatchedSendsOnly.getOrElse(false)
          ) if result.isFailure
          problem = result.failed.getOrElse(new Exception("Failed to extract error from Try"))
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
      // TODO: Refactor with Resource.
      kvm.shutdown.unsafeRunSync()
    }
    if (problems.nonEmpty) {
      System.exit(1)
    }
  }

  def mkRSpaceStoreManager[F[_]: Async: Log](
      dirPath: Path,
      mapSize: Long = 100 * mb
  ): F[KeyValueStoreManager[F]] = {
    // Specify database mapping
    val rspaceHistoryEnvConfig = LmdbEnvConfig(name = "history", mapSize)
    val rspaceColdEnvConfig    = LmdbEnvConfig(name = "cold", mapSize)
    val channelEnvConfig       = LmdbEnvConfig(name = "channels", mapSize)
    val dbMapping = Map[Db, LmdbEnvConfig](
      (Db("rspace-history"), rspaceHistoryEnvConfig),
      (Db("rspace-roots"), rspaceHistoryEnvConfig),
      (Db("rspace-cold"), rspaceColdEnvConfig),
      (Db("rspace-channels"), channelEnvConfig)
    )
    LmdbDirStoreManager[F](dirPath, dbMapping)
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
  def repl(runtime: RhoRuntime[IO]): Unit = {
    printPrompt()
    Option(scala.io.StdIn.readLine()) match {
      case Some(line) =>
        evaluate(runtime, line).unsafeRunSync()
      case None =>
        Console.println("\nExiting...")
        return
    }
    repl(runtime)
  }

  def processFile(
      conf: Conf,
      runtime: RhoRuntime[IO],
      fileName: String,
      quiet: Boolean,
      unmatchedSendsOnly: Boolean
  ): Try[Unit] = {
    val processTerm: Par => Try[Unit] =
      if (conf.binary()) writeBinary(fileName)
      else if (conf.text()) writeHumanReadable(fileName)
      else
        evaluatePar(
          runtime,
          Using.resource(Source.fromFile(fileName))(_.mkString),
          quiet,
          unmatchedSendsOnly
        )

    val source = reader(fileName)

    import coop.rchain.catscontrib.effect.implicits.sEval
    Compiler[Eval]
      .sourceToADT(source, Map.empty[String, Par])
      .attempt
      .value
      .toTry
      .flatMap(processTerm)
  }

  def evaluate(runtime: RhoRuntime[IO], source: String): IO[Unit] =
    runtime.evaluate(source).map {
      case EvaluateResult(_, Vector(), _) =>
      case EvaluateResult(_, errors, _) =>
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
  def waitForSuccess(evaluatorFuture: Future[EvaluateResult]): Unit =
    try {
      Await.ready(evaluatorFuture, 5.seconds).value match {
        case Some(Success(EvaluateResult(cost, errors, _))) =>
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
      runtime: RhoRuntime[IO],
      source: String,
      quiet: Boolean,
      unmatchedSendsOnly: Boolean
  )(
      par: Par
  ): Try[Unit] = {
    val evaluatorIO =
      for {
        _ <- IO.delay(if (!quiet) {
              printNormalizedTerm(par)
            })
        result <- runtime.evaluate(source)
      } yield result

    Try(waitForSuccess(evaluatorIO.unsafeToFuture())).map { _ok =>
      if (!quiet) {
        printStorageContents(runtime, unmatchedSendsOnly).unsafeRunSync()
      }
    }
  }
}
