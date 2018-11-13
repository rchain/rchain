package coop.rchain.node.api

import java.io.{Reader, StringReader}

import coop.rchain.models.Par
import coop.rchain.node.Effect
import coop.rchain.node.model.repl._
import coop.rchain.rholang.interpreter.Interpreter._
import coop.rchain.rholang.interpreter.{Runtime, _}
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.Task
import monix.execution.Scheduler
import coop.rchain.node._

private[api] class ReplGrpcService(runtime: Runtime[Effect], worker: Scheduler)
    extends ReplGrpcMonix.Repl {

  def exec(reader: Reader): Task[ReplResponse] =
    Task
      .coeval(buildNormalizedTerm(reader))
      .attempt
      .flatMap {
        case Left(er) =>
          er match {
            case _: InterpreterError => Task.now(s"Error: ${er.toString}")
            case th: Throwable       => Task.now(s"Error: $th")
          }
        case Right(term) =>
          runEvaluate(runtime, term).value.map {
            case Left(ex) => s"Caught boxed exception: $ex"
            case Right(EvaluateResult(cost, errors)) =>
              val errorStr =
                if (errors.isEmpty)
                  ""
                else
                  errors
                    .map(_.toString())
                    .mkString("Errors received during evaluation:\n", "\n", "\n")
              s"Deployment cost: $cost\n" +
                s"${errorStr}Storage Contents:\n ${StoragePrinter.prettyPrint(runtime.space.store)}"
          }
      }
      .map(ReplResponse(_))

  private def defer[A](task: Task[A]): Task[A] =
    Task.defer(task).executeOn(worker)

  def run(request: CmdRequest): Task[ReplResponse] =
    defer(exec(new StringReader(request.line)))

  def eval(request: EvalRequest): Task[ReplResponse] =
    defer(exec(new StringReader(request.program)))

  def runEvaluate(runtime: Runtime[Effect], term: Par): Effect[EvaluateResult] =
    for {
      _      <- Task.now(printNormalizedTerm(term)).toEffect
      result <- evaluate(runtime, term)
    } yield result

  private def printNormalizedTerm(normalizedTerm: Par): Unit = {
    Console.println("\nEvaluating:")
    Console.println(PrettyPrinter().buildString(normalizedTerm))
  }
}
