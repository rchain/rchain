package coop.rchain.rholang.interpreter

import monix.eval.{MVar, Task}

/**
  * Chan[A] is a reimplementation of
  * Scalaz's Chan[A] for Task.
  *
  * Originally, I wanted to define this interface
  * for the "Quote" case class, so that
  * we didn't need to maintain a separate "store"
  * mapping Quotes to Chans and could send on quotes
  * directly.
  *
  * My next step was to determine if un-quoting and
  * re-quoting a process would destroy the quote's
  * reader/writer queues. If not, we have a concise
  * store implementation.
  *
  * @tparam A The type of data to be sent over a channel
  */
abstract class Chan[A] {
  def read: Task[A]
  def write(data: A): Task[Unit]
}

object Chan {

  import MVarOps._

  private type ChannelStream[A] = MVar[ChItem[A]]

  def empty[A]: Task[Chan[A]] = {
    val hole = MVar.empty[ChItem[A]]
    Task now new ChanImpl[A](MVar(hole), MVar(hole))
  }

  private[this] case class ChItem[A](head: A, tail: ChannelStream[A])

  private[this] class ChanImpl[A](readVar: MVar[ChannelStream[A]], writeVar: MVar[ChannelStream[A]])
      extends Chan[A] {

    def write(a: A): Task[Unit] = {
      val newHole = MVar.empty[ChItem[A]]
      for {
        hole <- writeVar.take
        _    <- hole.put(ChItem(a, newHole))
        _    <- writeVar.put(newHole)
      } yield ()
    }

    def read: Task[A] =
      modify(readVar)(readEnd =>
        for {
          item <- readEnd.read
        } yield (item.tail, item.head))

  }
}

object MVarOps {
  def modify[A, B](mvar: MVar[A])(f: A => Task[(A, B)]): Task[B] =
    for {
      a       <- mvar.take
      tup     <- f(a)
      (_a, b) = tup
      _       <- mvar.put(_a)
    } yield b
}
