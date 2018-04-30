package coop.rchain.node

import cats.implicits._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib._
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import scala.concurrent._
import scala.concurrent.duration._

object Main {

  def main(args: Array[String]): Unit = {
    val conf = Conf(args)
    (conf.eval.toOption, conf.repl()) match {
      case (Some(fileName), _) => executeEvaluate(fileName, conf)
      case (None, true)        => executeRepl(conf)
      case (None, false)       => executeNode(conf)
    }
  }

  private def executeEvaluate(fileName: String, conf: Conf): Unit = {
    implicit val io: SchedulerService = Scheduler.io("my-io")
    val repl                          = new Repl(conf.grpcHost(), conf.grpcPort())
    println(repl.eval(fileName).unsafeRunSync)
  }

  private def executeRepl(conf: Conf): Unit = {
    implicit val io: SchedulerService = Scheduler.io("my-io")
    val repl                          = new Repl(conf.grpcHost(), conf.grpcPort())
    val recipe: Task[Unit] = for {
      _    <- Task.delay(print("rholang> "))
      line <- Task.delay(scala.io.StdIn.readLine())
      _ <- line.trim match {
            case ""   => Task.delay(print("\n"))
            case line => repl.run(line) >>= (output => Task.delay(println(output)))

          }
    } yield ()

    (Task.delay(println(repl.logo)) *> MonadOps.forever(recipe)).unsafeRunSync
  }

  private def executeNode(conf: Conf): Unit = {
    implicit val io: SchedulerService = Scheduler.io("my-io")

    new NodeRuntime(conf).nodeProgram.value.unsafeRunSync match {
      case Right(_) => ()
      case Left(commError) =>
        throw new Exception(commError.toString) // TODO use Show instance instead
    }
  }
}
