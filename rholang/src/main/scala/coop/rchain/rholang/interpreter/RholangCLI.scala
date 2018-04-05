package coop.rchain.rholang.interpreter

import java.io._
import java.nio.file.Files
import java.util.concurrent.TimeoutException

import cats.syntax.either._
import coop.rchain.models.{Channel, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}
import coop.rchain.rspace.{IStore, LMDBStore}
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import org.rogach.scallop.ScallopConf

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.{Failure, Success}

object RholangCLI {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("Rholang Mercury 0.2")
    banner("""
             |Takes in a rholang source file and
             |evaluates it.
             |
             |Options:
             |""".stripMargin)
    footer("\nWill add more options soon.")

    val binary = opt[Boolean](descr = "outputs binary protobuf serialization")
    val text   = opt[Boolean](descr = "outputs textual protobuf serialization")
    val file   = trailArg[String](required = false, descr = "Rholang source file")
    verify()
  }

  def main(args: Array[String]): Unit = {

    val conf = new Conf(args)
    if (conf.file.supplied) {
      val fileName: String = conf.file()
      val source           = reader(fileName)
      buildNormalizedTerm(source).foreach { (par: Par) =>
        if (conf.binary()) {
          writeBinary(fileName, par)
        } else if (conf.text()) {
          writeHumanReadable(fileName, par)
        } else {
          evaluate(par)
        }
      }
    } else {
      print("> ")
      repl()
    }
  }

  def reader(fileName: String): FileReader = new FileReader(fileName)
  def lexer(fileReader: Reader): Yylex     = new Yylex(fileReader)
  def parser(lexer: Yylex): parser         = new parser(lexer, lexer.getSymbolFactory())

  private def evaluate(sortedTerm: Par): Unit = {
    val runtime = Runtime.create()
    evaluate(runtime.reducer, runtime.store, sortedTerm)
  }

  def evaluator(reducer: Reduce[Task], normalizedTerm: Par): Task[Unit] =
    for {
      _ <- printTask(normalizedTerm)
      _ <- reducer.inj(normalizedTerm)
    } yield ()

  def evaluate(reducer: Reduce[Task],
               store: IStore[Channel, Seq[Channel], Seq[Channel], TaggedContinuation],
               normalizedTerm: Par): Unit = {

    val evaluatorFuture: CancelableFuture[Unit] = evaluator(reducer, normalizedTerm).runAsync
    keepTrying(evaluatorFuture, store)
  }

  private def repl(): Unit = {
    val runtime = Runtime.create()
    for (ln <- Source.stdin.getLines) {
      if (ln.isEmpty) {
        print("> ")
      } else {
        val normalizedTerm = buildNormalizedTerm(new StringReader(ln)).right.get
        evaluate(runtime.reducer, runtime.store, normalizedTerm)
        print("\n> ")
      }
    }
  }

  def buildNormalizedTerm(source: Reader): Either[String, Par] = {
    val term = buildAST(source)
    val inputs =
      ProcVisitInputs(Par(), DebruijnIndexMap[VarSort](), DebruijnLevelMap[VarSort]())
    val normalizedTerm: Either[String, ProcVisitOutputs] =
      normalizeTerm(term, inputs)

    normalizedTerm flatMap (nt =>
      ParSortMatcher
        .sortMatch(Some(nt.par))
        .term
        .fold[Either[String, Par]](Left("ParSortMatcher failed"))(p => Right(p)))
  }

  private def buildAST(source: Reader): Proc = {
    val lxr = lexer(source)
    val ast = parser(lxr)
    ast.pProc()
  }

  private def printTask(normalizedTerm: Par): Task[Unit] =
    Task {
      print("Evaluating:\n")
      println(PrettyPrinter().buildString(normalizedTerm))
    }

  @tailrec
  private def keepTrying(
      evaluatorFuture: CancelableFuture[Unit],
      persistentStore: IStore[Channel, Seq[Channel], Seq[Channel], TaggedContinuation]): Unit =
    try {
      Await.ready(evaluatorFuture, 5.seconds).value match {
        case Some(Success(_)) =>
          print("Storage Contents:\n")
          println(StoragePrinter.prettyPrint(persistentStore))
          print("\n> ")
        case Some(Failure(e)) => {
          println("Caught boxed exception: " + e)
          throw e
        }
        case None => throw new Error("Error: Future claimed to be ready, but value was None")
      }
    } catch {
      case _: TimeoutException => {
        println("This is taking a long time. Feel free to ^C and quit.")
        keepTrying(evaluatorFuture, persistentStore)
      }
      case e: Exception => throw e
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
}
