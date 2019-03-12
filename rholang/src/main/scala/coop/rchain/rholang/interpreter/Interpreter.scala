package coop.rchain.rholang.interpreter

import cats._
import cats.effect.Sync
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

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

  implicit def interpreter[F[_]](
      implicit M: Monad[F],
      S: Sync[F],
      C: _cost[F],
      E: _error[F]
  ): Interpreter[F] =
    new Interpreter[F] {

      def evaluate(runtime: Runtime[F], term: String): F[EvaluateResult] =
        evaluate(runtime, term, Cost.Max)

      def evaluate(runtime: Runtime[F], term: String, initialPhlo: Cost): F[EvaluateResult] = {
        implicit val rand: Blake2b512Random = Blake2b512Random(128)
        for {
          checkpoint <- runtime.space.createCheckpoint()
          res        <- injAttempt(runtime.reducer, runtime.errorLog, term, initialPhlo)
          _          <- if (res.errors.nonEmpty) runtime.space.reset(checkpoint.root) else M.pure(())
        } yield res
      }

      def injAttempt(
          reducer: ChargingReducer[F],
          errorLog: ErrorLog[F],
          term: String,
          initialPhlo: Cost
      )(implicit rand: Blake2b512Random): F[EvaluateResult] = {
        val parsingCost = accounting.parsingCost(term)
        ParBuilder[F].buildNormalizedTerm(term).attempt.flatMap {
          case Right(parsed) =>
            for {
              _         <- reducer.setPhlo(initialPhlo)
              _         <- charge[F](parsingCost).attempt
              result    <- reducer.inj(parsed).attempt
              phlosLeft <- reducer.phlo
              oldErrors <- errorLog.readAndClearErrorVector()
              newErrors = result.swap.toSeq.toVector
              allErrors = oldErrors |+| newErrors
            } yield EvaluateResult(initialPhlo - phlosLeft, allErrors)
          case Left(error) =>
            for {
              _ <- charge[F](parsingCost).attempt
            } yield EvaluateResult(parsingCost, Vector(error))
        }
      }

    }
}
