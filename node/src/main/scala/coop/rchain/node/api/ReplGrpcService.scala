package coop.rchain.node.api

import cats.effect.Sync
import cats.implicits._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.node.model.repl._
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.syntax._
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{RhoRuntime, _}
import monix.eval.Task
import monix.execution.Scheduler

object ReplGrpcService {
  def instance[F[_]: Sync: Taskable](
      runtime: RhoRuntime[F],
      worker: Scheduler
  ): ReplGrpcMonix.Repl =
    new ReplGrpcMonix.Repl {
      def exec(source: String, printUnmatchedSendsOnly: Boolean = false): F[ReplResponse] =
        Sync[F]
          .attempt(
            ParBuilder[F]
              .buildNormalizedTerm(source, Map.empty[String, Par])
          )
          .flatMap {
            case Left(er) =>
              er match {
                case _: InterpreterError => Sync[F].delay(s"Error: ${er.toString}")
                case th: Throwable       => Sync[F].delay(s"Error: $th")
              }
            case Right(term) =>
              for {
                _ <- Sync[F].delay(printNormalizedTerm(term))
                res <- {
                  implicit val rand = Blake2b512Random(10)
                  runtime.evaluate(source, Cost.UNSAFE_MAX, Map.empty[String, Par])
                }
                prettyStorage <- if (printUnmatchedSendsOnly)
                                  StoragePrinter.prettyPrintUnmatchedSends(runtime)
                                else StoragePrinter.prettyPrint(runtime)
                EvaluateResult(cost, errors) = res
              } yield {
                val errorStr =
                  if (errors.isEmpty)
                    ""
                  else
                    errors
                      .map(_.toString())
                      .mkString("Errors received during evaluation:\n", "\n", "\n")
                s"Deployment cost: $cost\n" +
                  s"${errorStr}Storage Contents:\n$prettyStorage"
              }
          }
          .map(ReplResponse(_))

      private def defer[A](task: F[A]): Task[A] =
        Task.defer(task.toTask).executeOn(worker)

      def run(request: CmdRequest): Task[ReplResponse] =
        defer(exec(request.line))

      def eval(request: EvalRequest): Task[ReplResponse] =
        defer(exec(request.program, request.printUnmatchedSendsOnly))

      private def printNormalizedTerm(normalizedTerm: Par): Unit = {
        Console.println("\nEvaluating:")
        Console.println(PrettyPrinter().buildString(normalizedTerm))
      }
    }
}
