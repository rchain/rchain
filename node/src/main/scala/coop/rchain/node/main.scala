package coop.rchain.node

import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException
import coop.rchain.models.Par
import coop.rchain.node.repl._

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._, ski._, TaskContrib._
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import scala.concurrent.{ExecutionContext, Future}

object Main {

  def main(args: Array[String]): Unit = {
    val conf = Conf(args)
    (conf.eval.toOption, conf.repl()) match {
      case (Some(fileName), _) => executeEvaluate(fileName, conf)
      case (None, true)        => executeRepl(conf)
      case (None, false)       => executeNode(conf)
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

  private def executeEvaluate(fileName: String, conf: Conf): Unit = {
    import monix.execution.Scheduler.Implicits.global
    val repl = new Repl(conf.grpcHost(), conf.grpcPort())
    repl.eval(fileName) >>= (result => Task.delay(println(result)))
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
