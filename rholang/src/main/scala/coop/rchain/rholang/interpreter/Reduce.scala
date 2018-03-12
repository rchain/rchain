package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models.{GPrivate => _, _}
import Substitute._
import Env._
import implicits._
import monix.eval.Task

// Notes: Caution, a type annotation is often needed for Env.

/** Reduce is a type-class for evaluating Rholang expressions.
  *
  * @tparam M The kind of Monad used for evaluation.
  * @tparam A The type of addresses.
  * @tparam C The type of channel.
  * @tparam D The type of data to be stored.
  */
trait Reduce[M[_], A, C, D] {
  def alloc(level: Int)(env: Env[A]): M[Env[A]]
  def produce(quote: C)(data: D)(env: Env[A]): M[Option[D]]
  def consume(quote: C)(data: D)(env: Env[A]): M[Option[D]]
  def eval(par: Par)(env: Env[Par]): Task[Unit]
}

object Reduce {

  sealed trait Data

  implicit def DeBruijnInterpreter: Reduce[Task, Par, Quote, Data] =
    new Reduce[Task, Par, Quote, Data] {

      /* Continuation */
      case class Abstraction(channels: Seq[Channel], par: Par, persistent: Boolean, env: Env[Par])
          extends Data

      /* Message */
      case class Concretion(pars: Seq[Par], persistent: Boolean, env: Env[Par]) extends Data

      def inj(par: Par): Task[Unit] =
        for { _ <- eval(par)(Env[Par]()) } yield ()

      /**
        * Giving (n: Int) to alloc indicates that n fresh addresses
        * are to be generated and added to the environment. The resulting
        * environment is lifted into monadic context.
        *
        * @param level The number of fresh addresses to create
        * @param env   The environment to be updated
        * @return A new environment with level incremented
        *         by # of fresh addresses
        */
      def alloc(level: Int)(env: Env[Par]): Task[Env[Par]] =
        Task now {
          (Env[Par]() /: (0 to level).toList) { (_env, _) =>
            val addr: Par = GPrivate()
            _env.put(addr)
          }
        }

      /** Sending data requires a trip to the store. Chan's
        * read vs. take will be the difference between persistent
        * and ephemeral inputs.
        *
        * @param quote The channel on which data is being sent.
        * @param data  A "concretion" representing an executed output process.
        * @param env   An environment marking the execution context.
        * @return An optional abstraction representing the possibility
        *         that the corresponding input process has already executed.
        */
      def produce(quote: Quote)(data: Data)(env: Env[Par]): Task[Option[Data]] = ???

      /** Dual to the produce function. Will also require a
        * trip to the store. Chan's read vs. take will be the
        * difference between persistent and ephemeral outputs.
        *
        * @param quote The channel on which data is being read.
        * @param data An "abstraction" representing an awaiting continuation.
        * @param env An environment marking the execution context.
        * @return An optional concretion representing the possibility
        *         that the corresponding output process has already executed.
        */
      def consume(quote: Quote)(data: Data)(env: Env[Par]): Task[Option[Data]] = ???

      /** Best Guess:
        *
        * Pattern matches a list of channels against
        * a list of Pars. Matches occur against the
        * environment of the input process receiving
        * the message. The function is effectively
        * a binding function.
        *
        * Kyle: If a pattern match is unsuccessful,
        * is the continuation placed back into
        * the environment?
        *
        * What is the form of the quote pattern?
        *
        * @param channels A sequence of channels representing a pattern
        * @param pars A sequence of processes to be matched
        * @param env An environment to add pattern variable bindings
        * @return An updated environment
        */
      def _match(channels: Seq[Channel])(pars: Seq[Par])(env: Env[Par]): Task[Env[Par]] = ???

      /**
        * Variable "evaluation" is an environment lookup, but
        * lookup of an unbound variable should be an error.
        * Task "now" indicates that no thread is forked.
        *
        * @param varue The variable to be evaluated
        * @param env   The environment (possibly) containing
        *              a binding for the given variable.
        * @return If the variable has a binding (par), lift the
        *         binding into the monadic context, else signal
        *         an exception.
        */
      def eval(varue: Var)(env: Env[Par]): Task[Par] =
        varue.varInstance match {
          case BoundVar(level) =>
            env.get(level) match {
              case Some(par) =>
                Task now par
              case None =>
                Task raiseError new IllegalStateException("Unbound variable")
            }
          case FreeVar(_) =>
            Task raiseError new IllegalStateException("Unbound variable")
        }

      /**
        * Evaluating a channel always returns a
        * quote. If a quote is given to be evaluated, the quote
        * is lifted into the monadic context. If a channel
        * variable is given, the variable is evaluated and
        * the resulting par is quoted.
        *
        * @param channel The channel to be evaluated
        * @param env An environment that (possibly) has
        *            a binding for channel
        * @return A quoted process or "channel value"
        */
      def eval(channel: Channel)(env: Env[Par]): Task[Quote] =
        channel.channelInstance match {
          case q @ Quote(_) =>
            Task now q
          case ChanVar(varue) =>
            for { par <- eval(varue)(env) } yield Quote(par)
        }

      /**
        * Eval is well-defined on channel variables provided
        * a binding exists for the variable. It should
        * always yield either a Par or an error.
        */
      def eval(drop: Eval)(env: Env[Par]): Task[Unit] =
        for {
          quote <- eval(drop.channel.get)(env)
          _     <- eval(quote.value)(env)
        } yield ()

      /**
        * The constructor "now", used above in alloc,
        * guarantees that no new thread is forked.
        * The flatMap call sequences alloc and eval -
        * so New executes sequentially.
        *
        * @param neu
        * @param env
        * @return
        */
      def eval(neu: New)(env: Env[Par]): Task[Unit] =
        for {
          _env <- alloc(neu.bindCount)(env)
          _    <- eval(neu.p.get)(_env)
        } yield ()

      def eval(expr: Expr)(env: Env[Par]): Task[Unit] = ???

      /** Kyle: Why is the pattern of the case expression:

          message MatchCase {
            Par pattern = 1;
            Par source = 2;
          }

        * a Par and not a list of channels as in the input
        * statement?
        *
        * @param mat A Match expression to be interpreted.
        * @param env An environment marking execution context.
        * @return Unit
        */
      def eval(mat: Match)(env: Env[Par]): Task[Unit] = ???

      /** WanderUnordered is the non-deterministic analogue
        * of traverse - it parallelizes eval.
        *
        * 1. For a Par, parallelize the interpretation of each list
        *    of processes in the Par. That's the outer wander.
        * 2. For each process list, parallelize the interpretation
        *    of each process in the list. That's the inner wander.
        *
        * @param par
        * @param env
        * @return
        */
      def eval(par: Par)(env: Env[Par]): Task[Unit] = ???

      def debug(msg: String): Unit = {
        val now = java.time.format.DateTimeFormatter.ISO_INSTANT
          .format(java.time.Instant.now)
          .substring(11, 23)
        val thread = Thread.currentThread.getName
        println(s"$now [$thread]" + "\n" + msg)
      }
    }
}
