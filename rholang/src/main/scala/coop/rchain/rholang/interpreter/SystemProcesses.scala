package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.{BindPattern, Channel, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.{produce, IStore}
import monix.eval.Task

object SystemProcesses {

  def stdout: Seq[Seq[Channel]] => Task[Unit] = { arg: Seq[Seq[Channel]] =>
    Task(Console.println(arg))
  }

  def stdoutAck(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(arg, ack)) =>
      Task(Console.println(arg)).flatMap { (_: Unit) =>
        produce(store, ack, Seq(Channel(Quote(Par.defaultInstance))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }

  def stderr: Seq[Seq[Channel]] => Task[Unit] = { arg: Seq[Seq[Channel]] =>
    Task(Console.err.println(arg))
  }

  def stderrAck(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(arg, ack)) =>
      Task(Console.err.println(arg)).flatMap { (_: Unit) =>
        produce(store, ack, Seq(Channel(Quote(Par.defaultInstance))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }
}
