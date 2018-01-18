package AbstractInterpreter

import cats.Monad

//Trace := StateT[WriterT[List,List,(S,A)]] = S => WriterT[List,List,(S,A)]] = S => (S,A) => List[(List[S],(S,A)] = List[(A,S,List[S])]

//Trace is a monad transformer that takes a state and returns a list of possible end-states,
//a value, A, and, for each end-state, a list of intermediate states.

case class Trace[S,A](reduce: S => List[(A, S, List[S])]) {

  def withFilter(pred: A => Boolean): Trace[S,A] = Trace {
    state =>
      for { triple <- reduce(state) ; if pred(triple._1) } yield triple
  }

  def map[B](f: A => B): Trace[S,B] = Trace {
    state0 =>
      for { (ret,st1,log) <- reduce(state0) } yield {
        (f(ret),st1,log)
      }
  }

  def flatMap[B](f: A => Trace[S, B]): Trace[S, B] = Trace {
    state0 =>
      for { (a, state1, log0) <- reduce(state0)
            (b, state2, log1) <- f(a).reduce(state1) } yield {
        (b, state2, log0 ++ log1)
      }
  }
}

object Trace {

  def get[S]: Trace[S, S] = {
    state { st => (st, st) }
  }

  def put[S]: S => Trace[S, Unit] = {
    state1 =>
      state { state0 => ((), state1) }
  }

  def tell[S]: List[S] => Trace[S, Unit] =
    log =>
      writer((), log)

  def listen[S, A](ma: Trace[S, A]): Trace[S, (A, List[S])] = Trace {
    state0 =>
      for {(ret, state1, log) <- ma.reduce(state0)} yield {
        ((ret, log), state1, log)
      }
  }

  def fromList[S, A](xs: List[A]): Trace[S, A] = Trace {
    state =>
      xs map { x => (x, state, List()) }
  }

  def writer[S, A](entry: (A, List[S])): Trace[S, A] = Trace {
    state =>
      List {
        (entry._1, state, entry._2)
      }
  }

  def state[S, A](f: S => (A, S)): Trace[S, A] = Trace {
    state0 =>
      f(state0) match {
        case (ret, state1) => List {
          (ret, state1, Nil)
        }
      }
  }

  //Insight on how to idiomatically define this type-class is appreciated.
  implicit def reduceMonad[S]: Monad[Trace[S, ?]] = {

    new Monad[Trace[S, ?]] {

      def pure[A](x: A): Trace[S, A] = Trace {
        state =>
          List {(x, state, Nil)}
      }

      def flatMap[A, B](ma: Trace[S, A])(f: A => Trace[S, B]): Trace[S, B] = ma.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => Trace[S, Either[A, B]]): Trace[S, B] = {
        def go(e: Either[A, B]): Trace[S, B] = e match {
          case Left(a1) => tailRecM(a1)(f)
          case Right(b) => pure[B](b)
        }
        f(a).flatMap(go)
      }
    }
  }
}
