package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.{Channel, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.{produce, IStore}
import monix.eval.Task

object SystemProcesses {

  def stdout: List[Seq[Channel]] => Task[Unit] = { arg: List[Seq[Channel]] =>
    Task(Console.println(arg))
  }

  def stdoutAck(store: IStore[Channel, Seq[Channel], Seq[Channel], TaggedContinuation],
                dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : List[Seq[Channel]] => Task[Unit] = {
    case List(Seq(arg, ack)) =>
      Task(Console.println(arg)).flatMap { (_: Unit) =>
        produce(store, ack, Seq(Channel(Quote(Par.defaultInstance))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }

  def stderr: List[Seq[Channel]] => Task[Unit] = { arg: List[Seq[Channel]] =>
    Task(Console.err.println(arg))
  }

  def stderrAck(store: IStore[Channel, Seq[Channel], Seq[Channel], TaggedContinuation],
                dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : List[Seq[Channel]] => Task[Unit] = {
    case List(Seq(arg, ack)) =>
      Task(Console.err.println(arg)).flatMap { (_: Unit) =>
        produce(store, ack, Seq(Channel(Quote(Par.defaultInstance))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }
}
