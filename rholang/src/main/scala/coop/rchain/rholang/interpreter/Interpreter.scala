package coop.rchain.rholang.interpreter

import cats.effect._
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.{InterpreterError, OutOfPhlogistonsError}

// TODO: Allow for only one error
final case class EvaluateResult(cost: Cost, errors: Vector[InterpreterError]) {
  val failed: Boolean    = errors.nonEmpty
  val succeeded: Boolean = !failed
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
      term: String,
      initialPhlo: Cost,
      normalizerEnv: Map[String, Par]
  )(implicit rand: Blake2b512Random): F[EvaluateResult]
}

object Interpreter {

  def apply[F[_]](implicit interpreter: Interpreter[F]): Interpreter[F] = interpreter

  implicit def interpreter[F[_]](
      implicit S: Sync[F],
      C: _cost[F],
      E: _error[F]
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
          injAttempt(runtime.reducer, term, initialPhlo, normalizerEnv).attempt >>= {
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
          term: String,
          initialPhlo: Cost,
          normalizerEnv: Map[String, Par]
      )(implicit rand: Blake2b512Random): F[EvaluateResult] = {
        val parsingCost = accounting.parsingCost(term)
        for {
          _          <- C.set(initialPhlo)
          _          <- charge[F](parsingCost)
          oopeOption <- E.getAndSet(none)
          evaluateResult <- oopeOption.fold(
                             ParBuilder[F].buildNormalizedTerm(term, normalizerEnv).attempt >>= {
                               case Right(parsed) =>
                                 for {
                                   _                      <- reducer.inj(parsed)
                                   phlosLeft              <- C.get
                                   interpreterErrorOption <- E.getAndSet(none)
                                   evaluateResult <- interpreterErrorOption.fold(
                                                      EvaluateResult(
                                                        initialPhlo - phlosLeft,
                                                        Vector.empty
                                                      ).pure[F]
                                                    ) {
                                                      case interpreterError: InterpreterError =>
                                                        EvaluateResult(
                                                          initialPhlo - phlosLeft,
                                                          Vector(interpreterError)
                                                        ).pure[F]
                                                      case throwable =>
                                                        throwable.raiseError[F, EvaluateResult]
                                                    }
                                 } yield evaluateResult
                               case Left(interpreterError: InterpreterError) =>
                                 EvaluateResult(parsingCost, Vector(interpreterError)).pure[F]
                               case Left(throwable) => throwable.raiseError[F, EvaluateResult]
                             }
                           ) {
                             case interpreterError: InterpreterError =>
                               EvaluateResult(parsingCost, Vector(interpreterError)).pure[F]
                             case throwable => throwable.raiseError[F, EvaluateResult]
                           }
        } yield evaluateResult
      }
    }
}
