package coop.rchain.rholang.interpreter

import java.io.{BufferedOutputStream, FileOutputStream, FileReader, Reader, StringReader}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeoutException

import cats.syntax.either._
import coop.rchain.models.{BindPattern, Channel, Par, TaggedContinuation, Var}
import coop.rchain.rholang.interpreter.implicits.VectorPar
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}
import coop.rchain.rspace.IStore
import monix.eval.Task
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
        buildNormalizedTerm(source) match {
          case Right(par) =>
            if (conf.binary()) {
              writeBinary(fileName, par)
            } else if (conf.text()) {
              writeHumanReadable(fileName, par)
            } else {
              val evaluatorFuture = evaluate(runtime.reducer, par).runAsync
              waitThenPrintStorageContents(evaluatorFuture, runtime.store)
            }
          case Left(error) =>
            error.printStackTrace(Console.err)
        }
      } else {
        repl(runtime)
      }
    } finally {
      runtime.store.close()
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
        buildNormalizedTerm(new StringReader(line)) match {
          case Right(par) =>
            val evaluatorFuture = evaluate(runtime.reducer, par).runAsync
            waitThenPrintStorageContents(evaluatorFuture, runtime.store)
          case Left(SyntaxError(msg)) =>
            // we don't want to print stack trace for syntax errors
            Console.err.print(msg)
          case Left(error) =>
            error.printStackTrace(Console.err)
        }
      case None =>
        Console.println("\nExiting...")
        return
    }
    repl(runtime)
  }

  def buildNormalizedTerm(source: Reader): Either[Throwable, Par] =
    try {
      for {
        term <- buildAST(source)
        inputs = ProcVisitInputs(VectorPar(),
                                 DebruijnIndexMap[VarSort](),
                                 DebruijnLevelMap[VarSort]())
        outputs <- normalizeTerm(term, inputs).leftMap(s => new Exception(s))
        par <- Either.fromOption(ParSortMatcher.sortMatch(Some(outputs.par)).term,
                                 new Exception("ParSortMatcher failed"))
      } yield par
    } catch {
      case th: Throwable => Left(th)
    }

  private def buildAST(source: Reader): Either[Throwable, Proc] =
    Either
      .catchNonFatal {
        val lxr = lexer(source)
        val ast = parser(lxr)
        ast.pProc()
      }
      .leftMap {
        case ex: Exception if ex.getMessage.toLowerCase.contains("syntax") =>
          SyntaxError(ex.getMessage)
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

  private def normalizeTerm(term: Proc,
                            inputs: ProcVisitInputs): Either[String, ProcVisitOutputs] = {
    val normalizedTerm = ProcNormalizeMatcher.normalizeMatch(term, inputs)
    if (normalizedTerm.knownFree.count > 0) {
      if (normalizedTerm.knownFree.wildcards.isEmpty) {
        val topLevelFreeList = normalizedTerm.knownFree.env.map {
          case (name, (_, _, line, col)) => s"$name at $line:$col"
        }
        Left(
          s"Top level free variables are not allowed: ${topLevelFreeList.mkString("", ", ", "")}.")
      } else {
        val topLevelWildcardList = normalizedTerm.knownFree.wildcards.map {
          case (line, col) => s"_ (wildcard) at $line:$col"
        }
        Left(
          s"Top level wildcards are not allowed: ${topLevelWildcardList.mkString("", ", ", "")}.")
      }
    } else Right(normalizedTerm)
  }

  final case class SyntaxError(msg: String) extends Exception(msg)
}
