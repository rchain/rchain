package coop.rchain.node.api

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.monix.Monixable
import coop.rchain.node.model.repl._
import coop.rchain.rholang.interpreter.Interpreter._
import coop.rchain.rholang.interpreter.compiler.ParBuilder
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{Runtime, _}
import coop.rchain.shared.syntax._
import monix.eval.Task
import monix.execution.Scheduler

object ReplGrpcService {

  def apply[F[_]: Monixable: Sync](runtime: Runtime[F], worker: Scheduler): ReplGrpcMonix.Repl =
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
                  implicit val c = runtime.cost
                  Interpreter[F].evaluate(runtime, source, Map.empty[String, Par])
                }
                prettyStorage <- if (printUnmatchedSendsOnly)
                                  StoragePrinter.prettyPrintUnmatchedSends(runtime.space)
                                else StoragePrinter.prettyPrint(runtime.space)
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
        task.toTask.executeOn(worker)

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
