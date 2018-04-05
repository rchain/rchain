package coop.rchain.node

import java.io.FileReader

import cats.implicits._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import coop.rchain.rholang.interpreter.{RholangCLI, Runtime}
import monix.eval.Task

object Main {

  def main(args: Array[String]): Unit = {
    val conf = Conf(args)
    (conf.eval.toOption, conf.repl()) match {
      case (Some(fileName), _) => evaluateFile(conf, fileName)
      case (None, true)        => executeRepl(conf)
      case (None, false)       => executeNode(conf)
    }
  }

  private def evaluateFile(conf: Conf, fileName: String): Unit = {
    import monix.execution.Scheduler.Implicits.global

    val runtime: Runtime   = Runtime.create(conf.data_dir(), conf.map_size())
    val source: FileReader = RholangCLI.reader(fileName)
    RholangCLI.buildNormalizedTerm(source) match {
      case Right(par) =>
        val evaluatorFuture = RholangCLI.evaluate(runtime.reducer, par).runAsync
        RholangCLI.waitThenPrintStorageContents(evaluatorFuture, runtime.store)
      case Left(error) =>
        Console.err.println(error)
    }
  }

  private def executeRepl(conf: Conf): Unit = {
    import monix.execution.Scheduler.Implicits.global

    val repl = new Repl(conf.grpcHost(), conf.grpcPort())
    val recipe: Task[Unit] = for {
      _    <- Task.delay(print("> "))
      line <- Task.delay(scala.io.StdIn.readLine())
      _ <- line.trim match {
            case ""   => Task.delay(print("\n"))
            case line => repl.run(line) >>= (output => Task.delay(println(output)))

          }
    } yield ()

    (Task.delay(println(repl.logo)) *> MonadOps.forever(recipe)).unsafeRunSync
  }

  private def executeNode(conf: Conf): Unit = {
    import monix.execution.Scheduler.Implicits.global
    new NodeRuntime(conf).nodeProgram.value.unsafeRunSync match {
      case Right(_) => ()
      case Left(commError) =>
        throw new Exception(commError.toString) // TODO use Show instance instead
    }
  }
}
