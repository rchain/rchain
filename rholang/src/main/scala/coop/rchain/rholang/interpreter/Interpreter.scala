package coop.rchain.rholang.interpreter

import java.io.Reader

import coop.rchain.rholang.interpreter.RholangCLI.{buildNormalizedTerm, evaluate}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import monix.eval.Task
import coop.rchain.rholang.interpreter.errors.InterpreterError

private class FailingTask[T](task: Task[Either[Throwable, T]]) {
  def raiseOnLeft =
    task.flatMap {
      case Left(err) => Task.raiseError(err)
      case Right(v)  => Task.now(v)
    }
}

object Interpreter {
  implicit private def toFailingTask[T](task: Task[Either[Throwable, T]]) = new FailingTask(task)

  def execute(runtime: Runtime, reader: Reader): Task[String] =
    for {
      term   <- Task.coeval(buildNormalizedTerm(reader)).attempt.raiseOnLeft
      errors <- evaluate(runtime, term).attempt.raiseOnLeft
      result <- if (errors.isEmpty)
                 Task.now(storageAsString(runtime))
               else
                 Task.raiseError(new RuntimeException(mkErrorMsg(errors)))
    } yield (result)

  private def storageAsString(runtime: Runtime) =
    s"Storage Contents:\n ${StoragePrinter.prettyPrint(runtime.space.store)}"

  private def mkErrorMsg(errors: Vector[InterpreterError]) =
    errors
      .map(_.toString())
      .mkString("Errors received during evaluation:\n", "\n", "\n")

}
