package coop.rchain.rholang.interpreter

import java.io.Reader

import cats.MonadError
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.models.rholang.sort.ParSortMatcher
import monix.eval.{Coeval, Task}
import coop.rchain.rholang.interpreter.errors.{
  InterpreterError,
  SyntaxError,
  UnrecognizedInterpreterError,
  UnrecognizedNormalizerError
}
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}

private class FailingTask[T](task: Task[Either[Throwable, T]]) {
  def raiseOnLeft =
    task.flatMap {
      case Left(err) => Task.raiseError(err)
      case Right(v)  => Task.now(v)
    }
}

object Interpreter {
  implicit private def toFailingTask[T](task: Task[Either[Throwable, T]]) = new FailingTask(task)

  private def lexer(fileReader: Reader): Yylex = new Yylex(fileReader)
  private def parser(lexer: Yylex): parser     = new parser(lexer, lexer.getSymbolFactory())

  def buildNormalizedTerm(source: Reader): Coeval[Par] =
    try {
      for {
        term    <- buildAST(source).fold(err => Coeval.raiseError(err), proc => Coeval.delay(proc))
        inputs  = ProcVisitInputs(VectorPar(), IndexMapChain[VarSort](), DebruijnLevelMap[VarSort]())
        outputs <- normalizeTerm[Coeval](term, inputs)
        par <- Coeval.delay(
                ParSortMatcher
                  .sortMatch(outputs.par)
                  .term)
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

  def execute(runtime: Runtime, reader: Reader): Task[Runtime] =
    for {
      term   <- Task.coeval(buildNormalizedTerm(reader)).attempt.raiseOnLeft
      errors <- evaluate(runtime, term).attempt.raiseOnLeft
      result <- if (errors.isEmpty)
                 Task.now(runtime)
               else
                 Task.raiseError(new RuntimeException(mkErrorMsg(errors)))
    } yield (result)

  def evaluate(runtime: Runtime, normalizedTerm: Par): Task[Vector[Throwable]] = {
    implicit val rand = Blake2b512Random(128)
    for {
      _      <- runtime.reducer.inj(normalizedTerm)
      errors <- Task.now(runtime.readAndClearErrorVector)
    } yield errors
  }

  private def mkErrorMsg(errors: Vector[Throwable]) =
    errors
      .map(_.toString())
      .mkString("Errors received during evaluation:\n", "\n", "\n")

}
