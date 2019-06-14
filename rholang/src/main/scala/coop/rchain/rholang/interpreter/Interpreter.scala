package coop.rchain.rholang.interpreter

import cats.effect._
import cats.implicits._
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rholang.interpreter.accounting._

final case class EvaluateResult(cost: Cost, errors: Vector[Throwable])

trait Interpreter[F[_]] {

  def evaluate(runtime: Runtime[F], term: String, deployerPk: Option[PublicKey]): F[EvaluateResult]
  def evaluate(
      runtime: Runtime[F],
      term: String,
      initialPhlo: Cost,
      deployerPk: Option[PublicKey]
  ): F[EvaluateResult]

  def injAttempt(
      reducer: ChargingReducer[F],
      errorLog: ErrorLog[F],
      term: String,
      initialPhlo: Cost,
      deployerPk: Option[PublicKey]
  )(implicit rand: Blake2b512Random): F[EvaluateResult]
}

object Interpreter {

  def apply[F[_]](implicit interpreter: Interpreter[F]): Interpreter[F] = interpreter

  implicit def interpreter[F[_]](
      implicit S: Sync[F],
      C: _cost[F]
  ): Interpreter[F] =
    new Interpreter[F] {

      def evaluate(
          runtime: Runtime[F],
          term: String,
          deployerPk: Option[PublicKey]
      ): F[EvaluateResult] =
        evaluate(runtime, term, Cost.UNSAFE_MAX, deployerPk)

      def evaluate(
          runtime: Runtime[F],
          term: String,
          initialPhlo: Cost,
          deployerPk: Option[PublicKey]
      ): F[EvaluateResult] = {
        implicit val rand: Blake2b512Random = Blake2b512Random(128)
        for {
          checkpoint <- runtime.space.createCheckpoint()
          res        <- injAttempt(runtime.reducer, runtime.errorLog, term, initialPhlo, deployerPk)
          _          <- if (res.errors.nonEmpty) runtime.space.reset(checkpoint.root) else S.unit
        } yield res
      }

      def injAttempt(
          reducer: ChargingReducer[F],
          errorLog: ErrorLog[F],
          term: String,
          initialPhlo: Cost,
          deployerPk: Option[PublicKey]
      )(implicit rand: Blake2b512Random): F[EvaluateResult] = {
        val parsingCost = accounting.parsingCost(term)
        for {
          _           <- C.set(initialPhlo)
          parseResult <- charge[F](parsingCost).attempt
          res <- parseResult match {
                  case Right(_) =>
                    ParBuilder[F].buildNormalizedTerm(term, deployerPk).attempt.flatMap {
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
        } yield res

      }

    }
}
