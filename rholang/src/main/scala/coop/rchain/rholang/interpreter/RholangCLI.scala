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
    val persistentStore = buildStore()
    val interp          = RholangOnlyDispatcher.create(persistentStore).reducer
    evaluate(interp, persistentStore, sortedTerm)
  }

  def repl() = {
    val persistentStore = buildStore()
    val interp          = RholangOnlyDispatcher.create(persistentStore).reducer
    for (ln <- Source.stdin.getLines) {
      if (ln.isEmpty) {
        print("> ")
      } else {
        val normalizedTerm = buildNormalizedTerm(new StringReader(ln)).get
        evaluate(interp, persistentStore, normalizedTerm)
      }
    }
  }

  def buildStore(): LMDBStore[Channel, Seq[Channel], Seq[Channel], TaggedContinuation] = {
    val dbDir = Files.createTempDirectory("rchain-storage-test-")
    LMDBStore.create[Channel, Seq[Channel], Seq[Channel], TaggedContinuation](dbDir,
                                                                              1024 * 1024 * 1024)
  }

  def buildNormalizedTerm(source: Reader): Option[Par] = {
    val term = buildAST(source)
    val inputs =
      ProcVisitInputs(Par(), DebruijnIndexMap[VarSort](), DebruijnLevelMap[VarSort]())
    val normalizedTerm: ProcVisitOutputs = normalizeTerm(term, inputs)
    ParSortMatcher.sortMatch(Some(normalizedTerm.par)).term
  }

  private def buildAST(source: Reader): Proc = {
    val lxr = lexer(source)
    val ast = parser(lxr)
    ast.pProc()
  }

  def evaluate(interpreter: Reduce[Task],
               store: IStore[Channel, Seq[Channel], Seq[Channel], TaggedContinuation],
               normalizedTerm: Par): Unit = {
    val evaluatorTask = for {
      _ <- printTask(normalizedTerm)
      _ <- interpreter.inj(normalizedTerm)
    } yield ()
    val evaluatorFuture: CancelableFuture[Unit] = evaluatorTask.runAsync
    keepTrying(evaluatorFuture, store)
    print("\n> ")
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
          StoragePrinter.prettyPrint(persistentStore)
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

  private def normalizeTerm(term: Proc, inputs: ProcVisitInputs) = {
    val normalizedTerm = ProcNormalizeMatcher.normalizeMatch(term, inputs)
    if (normalizedTerm.knownFree.count > 0) {
      if (normalizedTerm.knownFree.wildcards.isEmpty) {
        val topLevelFreeList = normalizedTerm.knownFree.env.map {
          case (name, (_, _, line, col)) => s"$name at $line:$col"
        }
        throw new Error(
          s"Top level free variables are not allowed: ${topLevelFreeList.mkString("", ", ", "")}.")
      } else {
        val topLevelWildcardList = normalizedTerm.knownFree.wildcards.map {
          case (line, col) => s"_ (wildcard) at $line:$col"
        }
        throw new Error(
          s"Top level wildcards are not allowed: ${topLevelWildcardList.mkString("", ", ", "")}.")
      }
    }
    normalizedTerm
  }
}
