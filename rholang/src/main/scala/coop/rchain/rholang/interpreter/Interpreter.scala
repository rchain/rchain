package coop.rchain.rholang.interpreter

import cats._
import cats.effect.Sync
import cats.implicits._
import coop.rchain.catscontrib.mtl.implicits._
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
      implicit S: Sync[F],
      C: _cost[F]
  ): Interpreter[F] =
    new Interpreter[F] {

      def evaluate(runtime: Runtime[F], term: String): F[EvaluateResult] =
        evaluate(runtime, term, Cost.UNSAFE_MAX)

      def evaluate(runtime: Runtime[F], term: String, initialPhlo: Cost): F[EvaluateResult] = {
        implicit val rand: Blake2b512Random = Blake2b512Random(128)
        for {
          checkpoint <- runtime.space.createCheckpoint()
          res        <- injAttempt(runtime.reducer, runtime.errorLog, term, initialPhlo)
          _          <- if (res.errors.nonEmpty) runtime.space.reset(checkpoint.root) else S.unit
        } yield res
      }

      def injAttempt(
          reducer: ChargingReducer[F],
          errorLog: ErrorLog[F],
          term: String,
          initialPhlo: Cost
      )(implicit rand: Blake2b512Random): F[EvaluateResult] = {
        val parsingCost = accounting.parsingCost(term)
        for {
          _           <- C.set(initialPhlo)
          parseResult <- charge[F](parsingCost).attempt
          res <- parseResult match {
                  case Right(_) =>
                    ParBuilder[F].buildNormalizedTerm(term).attempt.flatMap {
                      case Right(parsed) =>
                        for {
                          result    <- reducer.inj(parsed).attempt
                          phlosLeft <- C.inspect(identity)
                          oldErrors <- errorLog.readAndClearErrorVector()
                          newErrors = result.swap.toSeq.toVector
                          allErrors = oldErrors |+| newErrors
                        } yield EvaluateResult(initialPhlo - phlosLeft, allErrors)
                      case Left(error) =>
                        EvaluateResult(parsingCost, Vector(error)).pure[F]
                    }
                  case Left(error) =>
                    EvaluateResult(parsingCost, Vector(error)).pure[F]
                }
          _ <- C.get
        } yield res

      }

    }
}
