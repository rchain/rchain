package coop.rchain.node.api

import coop.rchain.models.Par
import coop.rchain.node.model.repl._
import coop.rchain.rholang.interpreter.Interpreter._
import coop.rchain.rholang.interpreter.{Runtime, _}
import coop.rchain.rholang.interpreter.error_handling.errors.InterpreterError
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.Task
import monix.execution.Scheduler

private[api] class ReplGrpcService(runtime: Runtime[Task], worker: Scheduler)
    extends ReplGrpcMonix.Repl {

  def exec(source: String, printUnmatchedSendsOnly: Boolean = false): Task[ReplResponse] =
    ParBuilder[Task]
      .buildNormalizedTerm(source, NormalizerEnv.Empty)
      .attempt
      .flatMap {
        case Left(er) =>
          er match {
            case _: InterpreterError => Task.now(s"Error: ${er.toString}")
            case th: Throwable       => Task.now(s"Error: $th")
          }
        case Right(term) =>
          for {
            _ <- Task.now(printNormalizedTerm(term))
            res <- {
              implicit val c = runtime.cost
              Interpreter[Task].evaluate(runtime, source, NormalizerEnv.Empty)
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

  private def defer[A](task: Task[A]): Task[A] =
    Task.defer(task).executeOn(worker)

  def run(request: CmdRequest): Task[ReplResponse] =
    defer(exec(request.line))

  def eval(request: EvalRequest): Task[ReplResponse] =
    defer(exec(request.program, request.printUnmatchedSendsOnly))

  private def printNormalizedTerm(normalizedTerm: Par): Unit = {
    Console.println("\nEvaluating:")
    Console.println(PrettyPrinter().buildString(normalizedTerm))
  }
}
