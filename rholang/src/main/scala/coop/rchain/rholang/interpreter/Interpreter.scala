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
        ParBuilder[F].buildNormalizedTerm(term).attempt.flatMap {
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

  }
}
