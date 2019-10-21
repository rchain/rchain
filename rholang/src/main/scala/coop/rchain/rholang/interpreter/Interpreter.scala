package coop.rchain.rholang.interpreter

import cats.effect._
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.{InterpreterError, OutOfPhlogistonsError}

final case class EvaluateResult(cost: Cost, errors: Vector[InterpreterError]) {
  val isFailed: Boolean = errors.nonEmpty
}

trait Interpreter[F[_]] {

  def evaluate(
      runtime: Runtime[F],
      term: String,
      normalizerEnv: Map[String, Par]
  ): F[EvaluateResult]

  def evaluate(
      runtime: Runtime[F],
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Par]
  ): F[EvaluateResult]

  def injAttempt(
      reducer: Reduce[F],
      errorLog: ErrorLog[F],
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Par]
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
          normalizerEnv: Map[String, Par]
      ): F[EvaluateResult] =
        evaluate(runtime, term, Cost.UNSAFE_MAX, normalizerEnv)

      def evaluate(
          runtime: Runtime[F],
          term: String,
          initialPhlo: Cost,
          normalizerEnv: Map[String, Par]
      ): F[EvaluateResult] = {
        implicit val rand: Blake2b512Random = Blake2b512Random(128)
        runtime.space.createSoftCheckpoint() >>= { checkpoint =>
          injAttempt(runtime.reducer, runtime.errorLog, term, initialPhlo, normalizerEnv).attempt >>= {
            case Right(evaluateResult) =>
              if (evaluateResult.errors.nonEmpty)
                runtime.space.revertToSoftCheckpoint(checkpoint).as(evaluateResult)
              else evaluateResult.pure[F]
            case Left(throwable) =>
              runtime.space.revertToSoftCheckpoint(checkpoint) >> throwable
                .raiseError[F, EvaluateResult]
          }
        }
      }

      def injAttempt(
          reducer: Reduce[F],
          errorLog: ErrorLog[F],
          term: String,
          initialPhlo: Cost,
          normalizerEnv: Map[String, Par]
      )(implicit rand: Blake2b512Random): F[EvaluateResult] = {
        val parsingCost = accounting.parsingCost(term)
        for {
          _           <- C.set(initialPhlo)
          parseResult <- charge[F](parsingCost).attempt
          evaluateResult <- parseResult match {
                             case Right(_) =>
                               ParBuilder[F].buildNormalizedTerm(term, normalizerEnv).attempt >>= {
                                 case Right(parsed) =>
                                   for {
                                     injResultEither   <- reducer.inj(parsed).attempt
                                     phlosLeft         <- C.get
                                     interpreterErrors <- errorLog.readAndClearErrorVector()
                                     evaluateResult <- injResultEither.swap.toOption match {
                                                        case Some(OutOfPhlogistonsError) =>
                                                          EvaluateResult(
                                                            initialPhlo - phlosLeft,
                                                            OutOfPhlogistonsError +: interpreterErrors
                                                          ).pure[F]
                                                        case Some(throwable) =>
                                                          throwable.raiseError[F, EvaluateResult]
                                                        case None =>
                                                          EvaluateResult(
                                                            initialPhlo - phlosLeft,
                                                            interpreterErrors
                                                          ).pure[F]
                                                      }
                                   } yield evaluateResult
                                 case Left(interpreterError: InterpreterError) =>
                                   EvaluateResult(parsingCost, Vector(interpreterError)).pure[F]
                                 case Left(throwable) => throwable.raiseError[F, EvaluateResult]
                               }
                             case Left(interpreterError: InterpreterError) =>
                               EvaluateResult(parsingCost, Vector(interpreterError)).pure[F]
                             case Left(throwable) => throwable.raiseError[F, EvaluateResult]
                           }
        } yield evaluateResult
      }
    }
}
