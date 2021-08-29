package coop.rchain.rholang.interpreter

import cats.effect._
import cats.syntax.all._
import cats.implicits
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.RholangMetricsSource
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.{
  AggregateError,
  InterpreterError,
  OutOfPhlogistonsError
}
import coop.rchain.metrics.implicits._
import coop.rchain.rholang.interpreter.builder.ADTBuilder

final case class EvaluateResult(cost: Cost, errors: Vector[InterpreterError]) {
  val failed: Boolean    = errors.nonEmpty
  val succeeded: Boolean = !failed
}

trait Interpreter[F[_]] {
  def injAttempt(
      reducer: Reduce[F],
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Par]
  )(implicit rand: Blake2b512Random): F[EvaluateResult]
}

class InterpreterImpl[F[_]: Sync: Span](implicit C: _cost[F]) extends Interpreter[F] {
  implicit val InterpreterMetricSource = Metrics.Source(RholangMetricsSource, "interpreter")

  // Internal helper exception to mark parser error
  case class ParserError(parseError: InterpreterError) extends Throwable

  override def injAttempt(
      reducer: Reduce[F],
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Par]
  )(implicit rand: Blake2b512Random): F[EvaluateResult] = {

    // Internal helper exception to mark parser error

    val parsingCost = accounting.parsingCost(term)
    val evaluationResult = for {
      _ <- Span[F].traceI("set-initial-cost") { C.set(initialPhlo) }
      _ <- Span[F].traceI("charge-parsing-cost") { charge[F](parsingCost) }
      parsed <- Span[F].traceI("build-normalized-term") {
                 ADTBuilder[F, String, Par]
                   .buildWithEnv(term, normalizerEnv)
                   .handleErrorWith {
                     case err: InterpreterError => ParserError(err).raiseError[F, Par]
                   }
               }
      _         <- Span[F].traceI("reduce-term") { reducer.inj(parsed) }
      phlosLeft <- C.get
    } yield EvaluateResult(initialPhlo - phlosLeft, Vector())

    // Convert InterpreterError(s) to EvaluateResult
    // - all other errors are rethrown (not valid interpreter errors)
    evaluationResult.handleErrorWith(
      error =>
        C.get >>= (
            phlosLeft => handleError(initialPhlo, parsingCost, initialPhlo - phlosLeft, error)
        )
    )
  }

  def handleError(
      initialCost: Cost,
      parsingCost: Cost,
      evalCost: Cost,
      error: Throwable
  ): F[EvaluateResult] =
    error match {
      // Parsing error consumes only parsing cost
      case ParserError(parseError: InterpreterError) =>
        EvaluateResult(parsingCost, Vector(parseError)).pure[F]

      // For Out Of Phlogistons error initial cost is used because evaluated cost can be higher
      // - all phlos are consumed
      case error: OutOfPhlogistonsError.type =>
        EvaluateResult(initialCost, Vector(error)).pure[F]

      // InterpreterError(s) - multiple errors are result of parallel execution
      case AggregateError(ipErrs, errs) if errs.isEmpty =>
        EvaluateResult(initialCost, ipErrs).pure[F]

      // Aggregated fatal errors are rethrown
      case error: AggregateError =>
        error.raiseError[F, EvaluateResult]

      // InterpreterError is returned as a result
      case error: InterpreterError =>
        EvaluateResult(initialCost, Vector(error)).pure[F]

      // Any other error is unexpected and it's fatal, rethrow
      case error: Throwable =>
        error.raiseError[F, EvaluateResult]
    }
}

object Interpreter {

  def apply[F[_]](implicit instance: Interpreter[F]): Interpreter[F] = instance

  def newIntrepreter[F[_]: Sync: Span](implicit cost: _cost[F]): Interpreter[F] =
    new InterpreterImpl[F]()

}
