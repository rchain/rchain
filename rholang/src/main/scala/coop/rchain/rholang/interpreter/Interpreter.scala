package coop.rchain.rholang.interpreter

import java.io.{Reader, StringReader}

import cats.effect.Sync
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount}
import coop.rchain.rholang.interpreter.errors.{
  InterpreterError,
  LexerError,
  SyntaxError,
  TopLevelFreeVariablesNotAllowedError,
  TopLevelLogicalConnectivesNotAllowedError,
  TopLevelWildcardsNotAllowedError,
  UnrecognizedInterpreterError
}
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}
import monix.eval.{Coeval, Task}

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

  implicit lazy val sync = implicitly[Sync[Coeval]]

  def buildNormalizedTerm(rho: String): Coeval[Par] = buildNormalizedTerm(new StringReader(rho))

  def buildNormalizedTerm(source: Reader): Coeval[Par] =
    try {
      for {
        term <- buildAST(source)
        par  <- buildPar(term)
      } yield par
    } catch {
      case th: Throwable => Coeval.raiseError(UnrecognizedInterpreterError(th))
    }

  def buildPar(proc: Proc): Coeval[Par] = {

    val inputs = ProcVisitInputs(VectorPar(), IndexMapChain[VarSort](), DebruijnLevelMap[VarSort]())
    for {
      outputs <- normalizeTerm(proc, inputs)
      sorted  <- Sortable[Par].sortMatch(outputs.par)
    } yield sorted.term
  }

  private def buildAST(source: Reader): Coeval[Proc] =
    Coeval
      .delay {
        val lxr = lexer(source)
        val ast = parser(lxr)
        ast.pProc()
      }
      .adaptError {
        case ex: Exception if ex.getMessage.toLowerCase.contains("syntax") =>
          SyntaxError(ex.getMessage)
        case e: Error if e.getMessage.startsWith("Unterminated string at EOF, beginning at") =>
          LexerError(e.getMessage)
        case e: Error if e.getMessage.startsWith("Illegal Character") => LexerError(e.getMessage)
        case e: Error if e.getMessage.startsWith("Unterminated string on line") =>
          LexerError(e.getMessage)
        case th => UnrecognizedInterpreterError(th)
      }

  private def normalizeTerm[M[_]](term: Proc, inputs: ProcVisitInputs)(
      implicit sync: Sync[M]
  ): M[ProcVisitOutputs] =
    ProcNormalizeMatcher.normalizeMatch[M](term, inputs).flatMap { normalizedTerm =>
      if (normalizedTerm.knownFree.count > 0) {
        if (normalizedTerm.knownFree.wildcards.isEmpty && normalizedTerm.knownFree.logicalConnectives.isEmpty) {
          val topLevelFreeList = normalizedTerm.knownFree.env.map {
            case (name, (_, _, line, col)) => s"$name at $line:$col"
          }
          sync.raiseError(
            TopLevelFreeVariablesNotAllowedError(topLevelFreeList.mkString("", ", ", ""))
          )
        } else if (normalizedTerm.knownFree.logicalConnectives.nonEmpty) {
          def connectiveInstanceToString(conn: ConnectiveInstance): String =
            if (conn.isConnAndBody) "/\\ (conjunction)"
            else if (conn.isConnOrBody) "\\/ (disjunction)"
            else if (conn.isConnNotBody) "~ (negation)"
            else conn.toString

          val connectives = normalizedTerm.knownFree.logicalConnectives
            .map {
              case (connType, line, col) => s"${connectiveInstanceToString(connType)} at $line:$col"
            }
            .mkString("", ", ", "")
          sync.raiseError(TopLevelLogicalConnectivesNotAllowedError(connectives))
        } else {
          val topLevelWildcardList = normalizedTerm.knownFree.wildcards.map {
            case (line, col) => s"_ (wildcard) at $line:$col"
          }
          sync.raiseError(
            TopLevelWildcardsNotAllowedError(topLevelWildcardList.mkString("", ", ", ""))
          )
        }
      } else normalizedTerm.pure[M]
    }

  def execute(runtime: Runtime, reader: Reader): Task[Runtime] =
    for {
      term   <- Task.coeval(buildNormalizedTerm(reader)).attempt.raiseOnLeft
      errors <- evaluate(runtime, term).map(_.errors).attempt.raiseOnLeft
      result <- if (errors.isEmpty)
                 Task.now(runtime)
               else
                 Task.raiseError(new RuntimeException(mkErrorMsg(errors)))
    } yield result

  def evaluate(runtime: Runtime, normalizedTerm: Par): Task[EvaluateResult] = {
    implicit val rand      = Blake2b512Random(128)
    val evaluatePhlosLimit = Cost(Integer.MAX_VALUE) //This is OK because evaluate is not called on deploy
    for {
      checkpoint <- runtime.space.createCheckpoint()
      _          <- runtime.reducer.setAvailablePhlos(evaluatePhlosLimit)
      _          <- runtime.reducer.inj(normalizedTerm)(rand)
      errors     <- Task.now(runtime.readAndClearErrorVector())
      leftPhlos  <- runtime.reducer.getAvailablePhlos()
      cost       = leftPhlos.copy(cost = evaluatePhlosLimit - leftPhlos.cost)
      _          <- if (errors.nonEmpty) runtime.space.reset(checkpoint.root) else Task.now(())
    } yield EvaluateResult(cost, errors)
  }

  private def mkErrorMsg(errors: Vector[Throwable]) =
    errors
      .map(_.toString())
      .mkString("Errors received during evaluation:\n", "\n", "\n")

  final case class EvaluateResult(cost: CostAccount, errors: Vector[Throwable])

}
