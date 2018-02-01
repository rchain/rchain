package interpreter

import Proc._
import Alias._
import cats.implicits._
import MachineState._
import interpreter.Trace._

import scala.collection.immutable.HashMap

//* When creating sample expressions, every name must be unique!*/
object Example extends App {

  def evaluate(proc: Proc): Unit = {
    val result = RhoInterface.reduce
      .run(MachineState(HashMap.empty[Channel, ChannelQueue], List(proc)))
      .run
    for { index <- result.indices } {
      println(
        "\n" + result(index)._1.mkString(
          "Trace " + (index.toInt + 1).toString + "\n" + "\n",
          "\n",
          "\n" + "Terminated" + "\n"))
    }
  }
}

case class MachineState(store: Store, runQueue: RunQueue) {
  override def toString: String =
    "Queue: " + runQueue.mkString(" | ") + "\n" + "Store: " + store.mkString(
      "{ ",
      ", ",
      " }") + "\n"
}

object MachineState {

  /*
    A ChannelQueue may be an empty queue, a list of readers, or a list of writers.
    It will never be both a list of readers and a list of writers.
  */ 
  sealed trait ChannelQueue

  case class ReaderQueue(x: Reader, xs: List[Reader]) extends ChannelQueue {
    override def toString: String =
      (x :: xs)
        .map {
          _.toString
        }
        .mkString("[", "][", "]")
  }

  case class WriterQueue(x: Writer, xs: List[Writer]) extends ChannelQueue {
    override def toString: String =
      (x :: xs)
        .map {
          _.toString
        }
        .mkString("[", "][", "]")
  }

  case object EmptyQueue extends ChannelQueue {
    override def toString: String = "[]"
  }

  /*
    A reader is a abstraction providing a bound name "z", and a continuation
    to evaluate, "k".
  */
  sealed trait Reader

  case class Abstraction(z: Quote, k: Proc) extends Reader {
    override def toString: String = " λ" + z + " { " + k.toString + " } "
  }

  //* A writer simply represents a message, "q".*/
  sealed trait Writer

  case class Concretion(q: Quote) extends Writer {
    override def toString: String = " " + q.toString + " "
  }

  def getStore: Trace[MachineState, List[MachineState], Store] = {
    get[MachineState, List[MachineState]].map { state =>
      state.store
    }
  }

  def putStore: Store => Trace[MachineState, List[MachineState], Unit] =
    store =>
      modify[MachineState, List[MachineState]] {
        case MachineState(_, runQueue) => MachineState(store, runQueue)
    }

  def getRunQueue: Trace[MachineState, List[MachineState], RunQueue] = {
    get[MachineState, List[MachineState]].map { st =>
      st.runQueue
    }
  }

  def putRunQueue: RunQueue => Trace[MachineState, List[MachineState], Unit] = {
    runQueue =>
      println("RunQueue: " + runQueue.mkString("{ ", ", ", " }") + "\n")
      modify[MachineState, List[MachineState]] {
        case MachineState(store, _) => MachineState(store, runQueue)
      }
  }

  //* Smart constructor for readerQueue */
  def readerQueue: List[Reader] => ChannelQueue = {
    case Nil          => EmptyQueue
    case reader :: rs => ReaderQueue(reader, rs)
  }

  //* Smart constructor for writerQueue */
  def writerQueue: List[Writer] => ChannelQueue = {
    case Nil          => EmptyQueue
    case writer :: ws => WriterQueue(writer, ws)
  }

}

object RhoInterface {

  import MachineState._

  /* 
    Variable binding is represented by substitution in the body of the process.
    A more efficient implementation will achieve the same result using environments.
  */
  def bind: Quote => Quote => Proc => Proc =
    atQ => z => proc => semanticSubstitution(proc)(atQ)(z)

  def write
    : Channel => ChannelQueue => Trace[MachineState, List[MachineState], Unit] =
    chan =>
      chanQ =>
        for {
          store <- getStore
          _ <- putStore(store + {
            chan -> chanQ
          })
        } yield ()

  def alloc
    : Channel => Trace[MachineState, List[MachineState], ChannelQueue] = {
    chan =>
      val e = EmptyQueue
      for { _ <- write(chan)(e) } yield e
  }

  def read: Channel => Trace[MachineState, List[MachineState], ChannelQueue] =
    chan =>
      for {
        store <- getStore
        chanQ1 <- store.get(chan) match {
          case Some(chanQ0) =>
            pure[MachineState, List[MachineState], ChannelQueue](chanQ0)
          case None => alloc(chan)
        }
      } yield chanQ1

  def reduce: Trace[MachineState, List[MachineState], Unit] = {

    //* Get run-queue and pull first process off.*/
    for {
      runQueue <- getRunQueue

      _ <- runQueue match {

        //* If the queue is empty, log the final state, and terminate.*/
        case Nil =>
          for {
            st <- get[MachineState, List[MachineState]]

            _ <- tell(List(st))
          } yield ()

        case proc :: xs =>
          for {
            _ <- proc match {

              //* (Store, 0 :: R) -> (Store, R) */
              case Zero =>
                for {
                  st <- get[MachineState, List[MachineState]]

                  _ <- tell(List(st))

                  _ <- putRunQueue(xs)

                } yield ()

              case par @ Par(_) =>
                /*
                  Encodes non-determinism by generating an auxiliary run-queue 
                  for every permutation of the set (P1 | ... | Pn)
                */
                for {
                  interleaving <- fromList[MachineState,
                                           List[MachineState],
                                           Seq[Proc]](
                    par.processes.permutations.toList)

                  //* Adds a permutation to the original run-queue*/
                  newRunQueue = (interleaving ++ xs).toList

                  //* Continues evaluating with new run-queue*/
                  _ <- putRunQueue(newRunQueue)

                } yield ()

              case Input(Action(x, z), k) =>
                val abs = Abstraction(z, k)

                for {
                  st <- get[MachineState, List[MachineState]]

                  _ <- tell(List(st))

                  chanQ <- read(x)

                  _ <- chanQ match {

                    //* If there is a writer waiting, pull it off, and bind it's message to z in k.*/
                    case WriterQueue(writer: Concretion, writers) =>
                      for {
                        _ <- write(x)(writerQueue(writers))

                        _ <- putRunQueue(bind(writer.q)(z)(k) :: xs)

                      } yield ()

                    //* If there is a reader waiting, create a reader, Abstraction(z,k), and add to the end of queue.*/
                    case ReaderQueue(reader, readers) =>
                      for {
                        _ <- write(x)(readerQueue((reader :: readers) :+ abs))

                        _ <- putRunQueue(xs)

                      } yield ()

                    //* If queue is empty, create a ReaderQueue, and add reader to it.*/
                    case EmptyQueue =>
                      for {
                        _ <- write(x)(readerQueue(List(abs)))

                        newStore <- getStore

                        _ <- set[MachineState, List[MachineState]](
                          MachineState(newStore, xs))

                      } yield ()
                  }
                } yield ()

              case Output(x, q) =>
                val atQ = Quote(q)

                for {
                  st <- get[MachineState, List[MachineState]]

                  _ <- tell(List(st))

                  chanQ <- read(x)

                  _ <- chanQ match {

                    //* Similar to ReaderQueue rule in Input.*/
                    case WriterQueue(writer: Concretion, writers) =>
                      for {
                        _ <- write(x)(
                          writerQueue((writer :: writers) :+ Concretion(atQ)))

                        newStore <- getStore

                        _ <- set[MachineState, List[MachineState]](
                          MachineState(newStore, xs))

                      } yield ()

                    //* If reader in the queue, pull it off, bind message, and add continuation to the end of the run-queue*/
                    case ReaderQueue(reader, readers) =>
                      reader match {

                        case Abstraction(z, k) =>
                          for {
                            _ <- write(x)(readerQueue(readers))

                            newStore <- getStore

                            _ <- set[MachineState, List[MachineState]](
                              MachineState(newStore, xs :+ bind(atQ)(z)(k)))

                          } yield ()
                      }

                    //* Similar to EmptyQueue rule in Input*/
                    case EmptyQueue =>
                      for {
                        _ <- write(x)(writerQueue(List(Concretion(atQ))))

                        newStore <- getStore

                        _ <- set[MachineState, List[MachineState]](
                          MachineState(newStore, xs))

                      } yield ()
                  }
                } yield ()

              case Drop(x) =>
                x match {

                  case Quote(p) =>
                    for {
                      st <- get[MachineState, List[MachineState]]

                      _ <- tell(List(st))

                      //* (Store, *@Q :: R) -> (Store, Q :: R) */
                      _ <- putRunQueue(p :: xs)
                    } yield ()

                }

              case _ => sys.error("Undefined term")
            }

            _ <- reduce

          } yield ()
      }
    } yield ()
  }
}
