package coop.rchain.rholang.interpreter

import java.io.{Reader, StringReader}

import cats.effect.Sync
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.{
  LexerError,
  OutOfPhlogistonsError,
  ParserError,
  SyntaxError,
  TopLevelFreeVariablesNotAllowedError,
  TopLevelLogicalConnectivesNotAllowedError,
  TopLevelWildcardsNotAllowedError,
  UnrecognizedInterpreterError
}
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}

final case class EvaluateResult(cost: Cost, errors: Vector[Throwable])

trait Interpreter[F[_]] {

  def buildNormalizedTerm(source: String): F[Par]

  def buildNormalizedTerm(reader: Reader): F[Par]

  def buildPar(proc: Proc): F[Par]

  def evaluate(runtime: Runtime[F], term: String): F[EvaluateResult]
  def evaluate(runtime: Runtime[F], term: String, initialPhlo: Cost): F[EvaluateResult]

  def injAttempt(
      reducer: ChargingReducer[F],
      errorLog: ErrorLog[F],
      term: String,
      initialPhlo: Cost
  )(implicit rand: Blake2b512Random): F[EvaluateResult]
}

object Interpreter {

  def apply[F[_]](implicit interpreter: Interpreter[F]): Interpreter[F] = interpreter

  implicit def interpreter[F[_]](implicit F: Sync[F]): Interpreter[F] = new Interpreter[F] {

    def buildNormalizedTerm(source: String): F[Par] =
      buildNormalizedTerm(new StringReader(source))

    def buildNormalizedTerm(reader: Reader): F[Par] =
      for {
        proc <- buildAST(reader)
        par  <- buildPar(proc)
      } yield par

    def buildPar(proc: Proc): F[Par] =
      for {
        par       <- normalizeTerm(proc)
        sortedPar <- Sortable[Par].sortMatch(par)
      } yield sortedPar.term

    def evaluate(runtime: Runtime[F], term: String): F[EvaluateResult] =
      evaluate(runtime, term, Cost.Max)

    def evaluate(runtime: Runtime[F], term: String, initialPhlo: Cost): F[EvaluateResult] = {
      implicit val rand: Blake2b512Random = Blake2b512Random(128)
      for {
        checkpoint <- runtime.space.createCheckpoint()
        res        <- injAttempt(runtime.reducer, runtime.errorLog, term, initialPhlo)
        _          <- if (res.errors.nonEmpty) runtime.space.reset(checkpoint.root) else F.unit
      } yield res
    }

    def injAttempt(
        reducer: ChargingReducer[F],
        errorLog: ErrorLog[F],
        term: String,
        initialPhlo: Cost
    )(implicit rand: Blake2b512Random): F[EvaluateResult] = {
      val parsingCost = accounting.parsingCost(term)
      // TODO: charge(parsingCost) so that it is visible in the cost log
      val phloAfterParsing = initialPhlo - parsingCost
      if (phloAfterParsing.value <= 0)
        EvaluateResult(parsingCost, Vector(OutOfPhlogistonsError)).pure[F]
      else
        Interpreter[F].buildNormalizedTerm(term).attempt.flatMap {
          case Right(parsed) =>
            for {
              _         <- reducer.setPhlo(phloAfterParsing)
              result    <- reducer.inj(parsed).attempt
              phlosLeft <- reducer.phlo
              oldErrors <- errorLog.readAndClearErrorVector()
              newErrors = result.swap.toSeq.toVector
              allErrors = oldErrors |+| newErrors
            } yield EvaluateResult(initialPhlo - phlosLeft, allErrors)
          case Left(error) =>
            for {
              phlosLeft <- reducer.phlo
            } yield EvaluateResult(parsingCost, Vector(error))
        }
    }

    private def buildAST(reader: Reader): F[Proc] =
      for {
        lexer  <- lexer(reader)
        parser <- parser(lexer)
        proc <- F.delay(parser.pProc()).adaptError {
                 case ex: Exception if ex.getMessage.startsWith("Syntax error") =>
                   SyntaxError(ex.getMessage)
                 case er: Error
                     if er.getMessage.startsWith("Unterminated string at EOF, beginning at") =>
                   LexerError(er.getMessage)
                 case er: Error if er.getMessage.startsWith("Illegal Character") =>
                   LexerError(er.getMessage)
                 case er: Error if er.getMessage.startsWith("Unterminated string on line") =>
                   LexerError(er.getMessage)
                 case th: Throwable => UnrecognizedInterpreterError(th)
               }
      } yield proc

    private def normalizeTerm(term: Proc): F[Par] =
      ProcNormalizeMatcher
        .normalizeMatch[F](
          term,
          ProcVisitInputs(VectorPar(), IndexMapChain[VarSort](), DebruijnLevelMap[VarSort]())
        )
        .flatMap { normalizedTerm =>
          if (normalizedTerm.knownFree.count > 0) {
            if (normalizedTerm.knownFree.wildcards.isEmpty && normalizedTerm.knownFree.logicalConnectives.isEmpty) {
              val topLevelFreeList = normalizedTerm.knownFree.env.map {
                case (name, (_, _, line, col)) => s"$name at $line:$col"
              }
              F.raiseError(
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
                  case (connType, line, col) =>
                    s"${connectiveInstanceToString(connType)} at $line:$col"
                }
                .mkString("", ", ", "")
              F.raiseError(TopLevelLogicalConnectivesNotAllowedError(connectives))
            } else {
              val topLevelWildcardList = normalizedTerm.knownFree.wildcards.map {
                case (line, col) => s"_ (wildcard) at $line:$col"
              }
              F.raiseError(
                TopLevelWildcardsNotAllowedError(topLevelWildcardList.mkString("", ", ", ""))
              )
            }
          } else normalizedTerm.pure[F].map(_.par)
        }

    /**
      * @note In lieu of a purely functional wrapper around the lexer and parser
      *       [[F.adaptError]] is used as a catch-all for errors thrown in their
      *       constructors.
      */
    private def lexer(fileReader: Reader): F[Yylex] =
      F.delay(new Yylex(fileReader)).adaptError {
        case th: Throwable => LexerError("Lexer construction error: " + th.getMessage)
      }

    private def parser(lexer: Yylex): F[parser] =
      F.delay(new parser(lexer, lexer.getSymbolFactory)).adaptError {
        case th: Throwable => ParserError("Parser construction error: " + th.getMessage)
      }
  }
}
