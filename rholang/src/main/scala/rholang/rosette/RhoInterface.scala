package AbstractInterpreter

import ADT._
import AbstractInterpreter.StateSpace._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.HashMap

/*
 * RhoInterface is generic in an analysis monad and a notion of address. The example
 * type-class in the RhoInterface companion object uses the Task monad (eqiv. Haskell's IO)
 * to simulate execution and IOAddr to simulate mutable channel references.
 *
 * Why make RhoInterface generic in addresses? The example below gives no analysis of the program - simply
 * an interpretation. In "Control-flow analysis of higher-order languages"(1991) Shivers introduces "Contours"
 * to represent the contents of memory addresses at different program points. With this genericity, we can
 * introduce contours to outfit our addresses with a notion of time, making the analysis context sensitive.
 *
 */

object Example extends App {

  val reducible = New("x",Par(Output("x",Par(Zero(),Zero())),Input("z","x",Drop("z"))))

  for { future <- RhoInterface.chanStore.reduce(Val(HashMap.empty,reducible)).runAsync } yield {
    future.env
  }

}

trait RhoInterface[M[_],A] {

  val send: A => Val[A] => M[Unit]
  val recv: A => M[Val[A]]
  val read: A => M[Quote]
  val bind: A => Quote => M[Unit]
  val alloc: String => M[A]
  val reduce: Val[A] => M[Val[A]]

}

object RhoInterface {

  implicit def chanStore: RhoInterface[Task, IOAddr] = {

    new RhoInterface[Task, IOAddr] {

      /*
       * The functions below are store operations ( Store : A -> Val[A] ). Store
       * interaction becomes an important part of program analysis, yet we'd prefer not to have to explicitly
       * thread it through the evaluation, so we factor the store and store interaction into the analysis monad. In this
       * case the JVM heap is the "store" and our addresses are mutable references holding channels. Simply put, when
       * we use the Task monad, we recover a concrete interpreter.
       *
       * If we wanted to recover a static analyzer, we'd need to maintain the store explicitly, so we would implement
       * the store as a hashmap and use the state monad to thread the store through the evaluation.
       */

      val send: IOAddr => Val[IOAddr] => Task[Unit] = {
        ioAddr =>
          value =>
            ioAddr.lookup.put(value)
      }

      val recv: IOAddr => Task[Val[IOAddr]] = {
        ioAddr =>
          ioAddr.lookup.take
      }

      val read: IOAddr => Task[Quote] =
        ioAddr =>
          Task now {
            ioAddr.lookup
          }

      /*
       * Addresses could easily be made immutable by introducing error conditions
       * on reassignment
       */

      val bind: IOAddr => Quote => Task[Unit] = {
        ioAddr =>
          name =>
            Task now {
              ioAddr.value = Some(name)
            }
      }

      val alloc: Var => Task[IOAddr] =
        name =>
          Task now {
            IOAddr {
              None
            }
          }

      /*
       * Reduce is the evaluation function. It will reduce a process expression given
       * an initial environment.
       */

      val reduce: Val[IOAddr] => Task[Val[IOAddr]] = {

        value =>

          (value.env, value.proc) match {

            //Proof of termination
            case (env, zero @ Zero()) => Task {

              debug(zero.toString)

              Val(env, Zero())

            }

            case (env, in @ Input(z, x, k)) =>

              debug(in.toString)

              recv (env(x)) flatMap { message =>

                //Addresses for bound variables are allocated lazily

                alloc (z) flatMap { address =>

                  //Quoting occurs after the (P x Env) pair is read from the channel
                  bind (address) (message.quote) flatMap { _ =>

                    //Runs the continuation with updated environment
                    reduce (Val(env + (z -> address),k))

                  }
                }
              }

            case (env, o @ Output(x, q)) =>

              debug(o.toString)

              send (env(x)) (Val(env, q)) flatMap { _ =>

                reduce (Val(env, Zero()))

              }

            case (env, par @ Par(p, q)) =>

              debug(par.toString)

              /*
               * Running the code shows the execution is deterministic, possibly because
               * evaluating such small expressions is faster than forking logical thread.
               */

              val P = Task fork { reduce (Val(env, p)) }

              val Q = Task fork { reduce (Val(env, q)) }

              //Reassemble resulting environments of P and Q resp.
              val kont = Task.mapBoth(P,Q){(state1,state2) => Val[IOAddr](state1.env ++: state2.env, Zero())}

              kont

            case (env, drop @ Drop(x)) =>

              debug(drop.toString)

              read (env(x)) flatMap { chan =>

                reduce (chan.unquote)

              }

            case (env, neu @ New(x,p)) =>

              debug(neu.toString)

              alloc (x) flatMap { address =>

                /*
                * Unfortunately, the Rho ADT does not include a means to test for structural
                * equivalence, and so generating names in a manner consistent with the theory is
                * not yet possible. In the mean time, names are generated by quoting an empty
                * environment/null process pair.
                */

                val value = Val[IOAddr](HashMap.empty,Zero()).quote

                // Fill the address with an initially empty channel
                bind (address) (value) flatMap { _ =>

                  // Update the environment and reduce the scoped expression
                  reduce (Val(env + (x -> address),p))

                }

              }

            case (env,_) => sys.error("Unrecognized term")

          }
      }
    }
  }

  def debug(msg: String): Unit = {
    val now = java.time.format.DateTimeFormatter.ISO_INSTANT
      .format(java.time.Instant.now)
      .substring(11, 23)
    val thread = Thread.currentThread.getName
    println(s"$now [$thread] $msg")
  }

}