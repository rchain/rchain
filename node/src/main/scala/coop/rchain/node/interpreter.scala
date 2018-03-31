package coop.rchain.node

import scala.concurrent.Await
import scala.concurrent.duration._

import coop.rchain.models.{Channel, ListChannel, Par}
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.Reduce.DebruijnInterpreter
import coop.rchain.rholang.interpreter.RholangCLI.{lexer, normalizeTerm, parser}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}
import coop.rchain.rspace.{IStore, LMDBStore, Serialize}

import java.io.{FileReader, Reader, StringReader}
import java.nio.file.Files

import scala.annotation.tailrec
import monix.execution.Scheduler.Implicits.global
import monix.execution.{CancelableFuture, Scheduler}
import java.util.concurrent.TimeoutException

import scala.io.Source
import scala.util.{Failure, Success}

import monix.eval.Task
import cats._, cats.data._, cats.implicits._

object InterpreterRuntime {

  def evaluateFile(fileName: String): Unit = {
    val persistentStore: LMDBStore[Channel, Seq[Channel], Seq[Channel], Par] = buildStore
    val interp                                                               = Reduce.makeInterpreter(persistentStore)
    val source                                                               = reader(fileName)
    val sortedTerm: Option[Par]                                              = buildNormalizedTerm(source)

    evaluate(interp, persistentStore, sortedTerm.get)
  }

  def repl = {
    val persistentStore: LMDBStore[Channel, Seq[Channel], Seq[Channel], Par] = buildStore
    val interp                                                               = Reduce.makeInterpreter(persistentStore)

    for (ln <- Source.stdin.getLines) {
      if (ln.isEmpty) {
        print("> ")
      } else {
        val normalizedTerm = buildNormalizedTerm(new StringReader(ln)).get
        evaluate(interp, persistentStore, normalizedTerm)
      }
    }
  }

  private def reader(fileName: String): FileReader = new FileReader(fileName)
  private def lexer(fileReader: Reader): Yylex     = new Yylex(fileReader)
  private def parser(lexer: Yylex): parser         = new parser(lexer, lexer.getSymbolFactory())

  private def evaluate(interpreter: DebruijnInterpreter,
                       store: IStore[Channel, Seq[Channel], Seq[Channel], Par],
                       normalizedTerm: Par): Unit = {
    val evaluatorTask = for {
      _ <- printTask(normalizedTerm)
      _ <- interpreter.inj(normalizedTerm)
    } yield ()
    val evaluatorFuture: CancelableFuture[Unit] = evaluatorTask.runAsync
    keepTrying(evaluatorFuture, store)
  }

  private def buildNormalizedTerm(source: Reader): Option[Par] = {
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

  private def printTask(normalizedTerm: Par): Task[Unit] =
    Task {
      print("Evaluating:\n")
      println(PrettyPrinter().buildString(normalizedTerm))
      print("\n> ")
    }

  @tailrec
  private def keepTrying(evaluatorFuture: CancelableFuture[Unit],
                         persistentStore: IStore[Channel, Seq[Channel], Seq[Channel], Par]): Unit =
    Await.ready(evaluatorFuture, 5.seconds).value match {
      case Some(Success(_)) =>
        print("Storage Contents:\n")
        StoragePrinter.prettyPrint(persistentStore)
        print("\n> ")
      case Some(Failure(e: TimeoutException)) =>
        println("This is taking a long time. Feel free to ^C and quit.")
        keepTrying(evaluatorFuture, persistentStore)
      case Some(Failure(e)) => throw e
      case None             => throw new Error("Error: Future claimed to be ready, but value was None")
    }
  private def buildStore = {
    implicit val serializer = Serialize.mkProtobufInstance(Channel)
    implicit val serializer2 = new Serialize[Seq[Channel]] {
      override def encode(a: Seq[Channel]): Array[Byte] =
        ListChannel.toByteArray(ListChannel(a))

      override def decode(bytes: Array[Byte]): Either[Throwable, Seq[Channel]] =
        Either.catchNonFatal(ListChannel.parseFrom(bytes).channels.toList)
    }
    implicit val serializer3 = Serialize.mkProtobufInstance(Par)

    val dbDir = Files.createTempDirectory("rchain-storage-test-")
    LMDBStore.create[Channel, Seq[Channel], Seq[Channel], Par](dbDir, 1024 * 1024 * 1024)
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
