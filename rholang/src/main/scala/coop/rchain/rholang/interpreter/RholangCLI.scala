package coop.rchain.rholang.interpreter

import java.io.{BufferedOutputStream, FileOutputStream, FileReader, Reader, StringReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeoutException

import cats.data.EitherT
import cats.{Monad, _}
import cats.implicits._
import cats.syntax._
import coop.rchain.catscontrib.Capture._
import coop.rchain.models.{BindPattern, Channel, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.implicits.VectorPar
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}
import coop.rchain.rspace.IStore
import monix.eval.{Coeval, Task}
import monix.execution.{CancelableFuture, Scheduler}
import org.rogach.scallop.ScallopConf

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

    val data_dir = opt[Path](required = false,
                             descr = "Path to data directory",
                             default = Some(Files.createTempDirectory("rspace-store-")))

    val map_size = opt[Long](required = false,
                             descr = "Map size (in bytes)",
                             default = Some(1024L * 1024L * 1024L))

    val file = trailArg[String](required = false, descr = "Rholang source file")

    verify()
  }

  def main(args: Array[String]): Unit = {
    import monix.execution.Scheduler.Implicits.global

    val conf = new Conf(args)

    val runtime = Runtime.create(conf.data_dir(), conf.map_size())

    try {
      if (conf.file.supplied) {
        val fileName: String = conf.file()
        val source           = reader(fileName)
        buildNormalizedTerm(source).runAttempt match {
          case Right(par) =>
            if (conf.binary()) {
              writeBinary(fileName, par)
            } else if (conf.text()) {
              writeHumanReadable(fileName, par)
            } else {
              val evaluatorFuture = evaluate(runtime.reducer, par).runAsync
              waitThenPrintStorageContents(evaluatorFuture, runtime.space.store)
            }
          case Left(error) =>
            System.err.println(error)
        }
      } else {
        repl(runtime)
      }
    } finally {
      runtime.close()
    }
  }

  def reader(fileName: String): FileReader = new FileReader(fileName)
  def lexer(fileReader: Reader): Yylex     = new Yylex(fileReader)
  def parser(lexer: Yylex): parser         = new parser(lexer, lexer.getSymbolFactory())

  private def printPrompt(): Unit =
    Console.print("\nrholang> ")

  private def printNormalizedTerm(normalizedTerm: Par): Unit = {
    Console.println("\nEvaluating:")
    Console.println(PrettyPrinter().buildString(normalizedTerm))
  }

  private def printStorageContents(
      store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation]): Unit = {
    Console.println("\nStorage Contents:")
    Console.println(StoragePrinter.prettyPrint(store))
  }

  def evaluate(reducer: Reduce[Task], normalizedTerm: Par): Task[Unit] =
    for {
      _ <- Task.now(printNormalizedTerm(normalizedTerm))
      _ <- reducer.inj(normalizedTerm)
    } yield ()

  @tailrec
  def repl(runtime: Runtime)(implicit scheduler: Scheduler): Unit = {
    printPrompt()
    Option(scala.io.StdIn.readLine()) match {
      case Some(line) =>
        buildNormalizedTerm(new StringReader(line)).runAttempt match {
          case Right(par) =>
            val evaluatorFuture = evaluate(runtime.reducer, par).runAsync
            waitThenPrintStorageContents(evaluatorFuture, runtime.space.store)
          case Left(ie: InterpreterError) =>
            // we don't want to print stack trace for user errors
            Console.err.print(ie.toString)
          case Left(th) =>
            th.printStackTrace(Console.err)
        }
      case None =>
        Console.println("\nExiting...")
        return
    }
    repl(runtime)
  }

  def buildNormalizedTerm(source: Reader): Coeval[Par] =
    try {
      for {
        term <- buildAST(source).fold(err => Coeval.raiseError(err), proc => Coeval.delay(proc))
        inputs = ProcVisitInputs(VectorPar(),
                                 DebruijnIndexMap[VarSort](),
                                 DebruijnLevelMap[VarSort]())
        outputs <- normalizeTerm[Coeval](term, inputs)
        par <- ParSortMatcher
                .sortMatch[Coeval](Some(outputs.par))
                .map(_.term)
                .flatMap {
                  case None      => Coeval.raiseError[Par](SortMatchError("ParSortMatcher failed"))
                  case Some(par) => Coeval.delay(par)
                }
      } yield par
    } catch {
      case th: Throwable => Coeval.raiseError(UnrecognizedInterpreterError(th))
    }

  private def buildAST(source: Reader): Either[InterpreterError, Proc] =
    Either
      .catchNonFatal {
        val lxr = lexer(source)
        val ast = parser(lxr)
        ast.pProc()
      }
      .leftMap {
        case ex: Exception if ex.getMessage.toLowerCase.contains("syntax") =>
          SyntaxError(ex.getMessage)
        case th => UnrecognizedInterpreterError(th)
      }

  @tailrec
  def waitThenPrintStorageContents(
      evaluatorFuture: CancelableFuture[Unit],
      store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation]): Unit =
    try {
      Await.ready(evaluatorFuture, 5.seconds).value match {
        case Some(Success(_)) => printStorageContents(store)
        case Some(Failure(e)) => throw e
        case None             => throw new Exception("Future claimed to be ready, but value was None")
      }
    } catch {
      case _: TimeoutException =>
        Console.println("This is taking a long time. Feel free to ^C and quit.")
        waitThenPrintStorageContents(evaluatorFuture, store)
      case e: Exception =>
        throw e
    }

  private def writeHumanReadable(fileName: String, sortedTerm: Par): Unit = {
    val compiledFileName = fileName.replaceAll(".rho$", "") + ".rhoc"
    new java.io.PrintWriter(compiledFileName) {
      write(sortedTerm.toString)
      close()
    }
    println(s"Compiled $fileName to $compiledFileName")
  }

  private def writeBinary(fileName: String, sortedTerm: Par): Unit = {
    val binaryFileName = fileName.replaceAll(".rho$", "") + ".bin"
    val output         = new BufferedOutputStream(new FileOutputStream(binaryFileName))
    output.write(sortedTerm.toByteString.toByteArray)
    output.close()
    println(s"Compiled $fileName to $binaryFileName")
  }

  private def normalizeTerm[M[_]](term: Proc, inputs: ProcVisitInputs)(
      implicit err: MonadError[M, InterpreterError]): M[ProcVisitOutputs] =
    ProcNormalizeMatcher.normalizeMatch[M](term, inputs).flatMap { normalizedTerm =>
      if (normalizedTerm.knownFree.count > 0) {
        if (normalizedTerm.knownFree.wildcards.isEmpty) {
          val topLevelFreeList = normalizedTerm.knownFree.env.map {
            case (name, (_, _, line, col)) => s"$name at $line:$col"
          }
          err.raiseError(UnrecognizedNormalizerError(
            s"Top level free variables are not allowed: ${topLevelFreeList.mkString("", ", ", "")}."))
        } else {
          val topLevelWildcardList = normalizedTerm.knownFree.wildcards.map {
            case (line, col) => s"_ (wildcard) at $line:$col"
          }
          err.raiseError(UnrecognizedNormalizerError(
            s"Top level wildcards are not allowed: ${topLevelWildcardList.mkString("", ", ", "")}."))
        }
      } else normalizedTerm.pure[M]
    }
}
