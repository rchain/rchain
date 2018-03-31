package coop.rchain.node

import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent.TimeoutException

import coop.rchain.models.Par

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._, ski._, TaskContrib._
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}

object Main {

  def main(args: Array[String]): Unit = {
    val conf = Conf(args)
    // TODO each should return a Task (or effect) that we can execute with unsafeRunSync
    (conf.eval.toOption, conf.repl()) match {
      case (Some(fileName), _) => InterpreterRuntime.evaluateFile(fileName)
      case (None, true)        => executeREPL(conf)
      case (None, false)       => executeP2p(conf)
    }
  }

  private def executeREPL(conf: Conf): Unit = {
    print("> ")
    InterpreterRuntime.repl
  }

  private def executeP2p(conf: Conf): Unit = {
    import monix.execution.Scheduler.Implicits.global
    new NodeRuntime(conf).recipe.value.unsafeRunSync {
      case Right(_) => ()
      case Left(commError) =>
        throw new Exception(commError.toString) // TODO use Show instance instead
    }
  }
}
