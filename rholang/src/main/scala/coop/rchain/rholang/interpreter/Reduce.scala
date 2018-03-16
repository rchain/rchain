package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.{GPrivate => _, _}

import Substitute._
import Env._
import implicits._

import cats.{Eval => _, _}
import cats.data._
import cats.implicits._

import monix.eval.{MVar, Task}
import scala.collection.mutable.HashMap

// Notes: Caution, a type annotation is often needed for Env.

/** Reduce is a type-class for evaluating Rholang expressions.
  *
  * @tparam M The kind of Monad used for evaluation.
  * @tparam C The type of a channel.
  * @tparam A The type of data in the environment.
  * @tparam P The type of a pattern.
  * @tparam B The type of a read body
  */
trait Reduce[M[_], C, A, P, B] {
  type Cont[Data, Body] = (Body, Env[Data])
  def produce(chan: C, data: Seq[A], persistent: Boolean)(
      implicit env: Env[A]): M[Option[Cont[A, B]]]
  def consume(binds: Seq[(Seq[P], C)], body: B, persistent: Boolean)(
      implicit env: Env[A]): M[Option[Cont[A, B]]]
  def eval(par: Par)(implicit env: Env[Par]): Task[Unit]
  def continue(b: B)(implicit env: Env[Par]): Task[Unit]
}

object Reduce {
  // TODO: How do I not define this twice?
  type Cont[Data, Body] = (Body, Env[Data])
  // Doesn't do joins. Only handles var patterns and @Var patterns.
  // Does handle arity matching.
  // The map is a map from a channel (Quote), to a pair.
  // The pair is a list of writes and a list of reads.
  // a write is just a list of Pars.
  // a read is a list of Channels, which for early simplicity must be either:
  // a Chanvar(FreeVar) or a Quote(EVar())
  class SimpleTuplespace(
      space: HashMap[Quote,
                     (Seq[(Seq[Par], Boolean)], Seq[(Seq[Channel], Cont[Par, Par], Boolean)])]) {
    def this() =
      this(
        HashMap
          .empty[Quote, (Seq[(Seq[Par], Boolean)], Seq[(Seq[Channel], Cont[Par, Par], Boolean)])])
    val spaceMVar = MVar(space)

    def simpleProduce(c: Quote, data: Seq[Par], persistent: Boolean)(
        implicit env: Env[Par]): Task[Option[Cont[Par, Par]]] =
      for {
        space <- spaceMVar.take
        result <- {
          space.get(c) match {
            case None => {
              space.put(
                c,
                (Seq((data, persistent)), Seq.empty[(Seq[Channel], Cont[Par, Par], Boolean)]))
              Task.pure(None)
            }
            case Some((writes, reads)) => {
              val (head, tail) = reads.span({
                case (pattern, _, _) => (pattern.length != data.length)
              })
              tail match {
                case Nil => {
                  // TODO: Same as below, the data write should be substituted, at least for now.
                  space.put(c, ((data, persistent) +: writes, reads))
                  Task.pure(None)
                }
                case (binds, cont: Cont[Par, Par], readPersistent) +: remReads => {
                  val newWrites =
                    if (persistent)
                      (data, persistent) +: writes
                    else
                      writes
                  val newReads =
                    if (readPersistent) // persistent
                      head ++ tail
                    else
                      head ++ remReads
                  space.put(c, (newWrites, newReads))
                  // TODO: data may refer to the environment. Make it fully substituted.
                  Task.pure(Some((cont._1, cont._2.put(data))))
                }
              }
            }
          }
        }
        _ <- spaceMVar.put(space)
      } yield result

    def simpleConsume(c: Quote, pattern: Seq[Channel], body: Par, persistent: Boolean)(
        implicit env: Env[Par]): Task[Option[Cont[Par, Par]]] =
      for {
        space <- spaceMVar.take
        result <- {
          space.get(c) match {
            case None => {
              space.put(c,
                        (Seq.empty[(Seq[Par], Boolean)], Seq((pattern, (body, env), persistent))))
              Task.pure(None)
            }
            case Some((writes, reads)) => {
              val (head, tail) = writes.span({
                case (data, _) => (pattern.length != data.length)
              })
              tail match {
                case Nil => {
                  space.put(c, (writes, (pattern, (body, env), persistent) +: reads))
                  Task.pure(None)
                }
                case (data, writePersistent) +: remWrites => {
                  val newReads =
                    if (persistent)
                      (pattern, (body, env), persistent) +: reads
                    else
                      reads
                  val newWrites =
                    if (writePersistent)
                      head ++ tail
                    else
                      head ++ remWrites
                  space.put(c, (newWrites, newReads))
                  // TODO: should writes have their own environment?
                  Task.pure(Some((body, env.put(data))))
                }
              }
            }
          }
        }
        _ <- spaceMVar.put(space)
      } yield result
  }

  // implicit def DebruijnInterpreter: Reduce[Task, Quote, Par, Channel, Par] =
  object debruijnInterpreter extends Reduce[Task, Quote, Par, Channel, Par] {
    val tupleSpace = new SimpleTuplespace()

    // This makes sense. Run a Par in the empty environment.
    def inj(par: Par): Task[Unit] =
      for { _ <- eval(par)(Env[Par]()) } yield ()

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
    def produce(chan: Quote, data: Seq[Par], persistent: Boolean)(
        implicit env: Env[Par]): Task[Option[Cont[Par, Par]]] =
      tupleSpace.simpleProduce(chan, data, persistent)

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
    def consume(binds: Seq[(Seq[Channel], Quote)], body: Par, persistent: Boolean)(
        implicit env: Env[Par]): Task[Option[Cont[Par, Par]]] =
      binds match {
        case Nil         => Task raiseError new Error("Error: empty binds")
        case bind +: Nil => tupleSpace.simpleConsume(bind._2, bind._1, body, persistent)
        case _           => Task raiseError new Error("Error: joins are not currently implemented.")
      }

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
    def eval(varue: Var)(implicit env: Env[Par]): Task[Par] =
      varue.varInstance match {
        case BoundVar(level) =>
          env.get(level) match {
            case Some(par) =>
              Task.pure(par)
            case None =>
              Task raiseError new IllegalStateException("Unbound variable")
          }
        case FreeVar(_) =>
          Task raiseError new IllegalStateException(
            "Unbound variable: attempting to evaluate a pattern")
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
    def eval(channel: Channel)(implicit env: Env[Par]): Task[Quote] =
      channel.channelInstance match {
        case q @ Quote(_) =>
          Task now q
        case ChanVar(varue) =>
          for { par <- eval(varue)(env) } yield Quote(par)
      }

    /** Algorithm as follows:
      *
      * 1. Fully substitute the send statement in
      *    the given environment
      * 2. Retrieve "value" (binding) of the subject channel
      * 3. Produce on the channel value:
      *     - If a continuation exists at the channel,
      *       match sender pars against abstraction patterns.
      *       Pattern bindings are added to the sender environment.
      *     - If a continuation does not exist at the channel,
      *       return unit.
      * 4. Merge the abstraction environment with the updated
      *    sender environment.
      * 5. Interpreter the body of the abstraction in the
      *    new environment.
      * @param send An output process
      * @param env0 An execution context
      * @return
      */
    def eval(send: Send)(implicit env: Env[Par]): Task[Unit] =
      for {
        quote           <- eval(send.chan.get)
        data            <- send.data.toList.traverse(x => exprEval(x)(env))
        subChan: Quote  = substitute(quote)
        optContinuation <- produce(subChan, data, send.persistent)

        _ <- optContinuation match {
              case Some((body: Par, newEnv: Env[Par])) =>
                continue(body)(newEnv)
              case None => Task now { () }
            }
      } yield ()

    def eval(receive: Receive)(implicit env: Env[Par]): Task[Unit] =
      for {
        binds <- receive.binds
                  .map((rb: ReceiveBind) =>
                    eval(rb.source.get).map(quote => (rb.patterns, substitute(quote))))
                  .toList
                  .sequence
        optContinuation <- consume(binds, receive.body.get, receive.persistent)
        _ <- optContinuation match {
              case Some((body: Par, newEnv: Env[Par])) =>
                continue(body)(newEnv)
              case None => Task now { () }
            }
      } yield ()

    /**
      * Eval is well-defined on channel variables provided
      * a binding exists for the variable. It should
      * always yield either a Par or an error.
      */
    def eval(drop: Eval)(implicit env: Env[Par]): Task[Unit] =
      for {
        quote <- eval(drop.channel.get)
        _     <- eval(quote.value)
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
    def eval(neu: New)(implicit env: Env[Par]): Task[Unit] = {
      def alloc(level: Int)(implicit env: Env[Par]): Task[Env[Par]] =
        Task now {
          (Env[Par]() /: (0 to level).toList) { (_env, _) =>
            val addr: Par = GPrivate()
            _env.put(addr)
          }
        }
      for {
        _env <- alloc(neu.bindCount)
        _    <- eval(neu.p.get)
      } yield ()
    }

    def evalToInt(p: Par)(implicit env: Env[Par]): Task[Option[Int]] = {
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.evals.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
        None
      p.exprs match {
        case Expr(GInt(v)) +: Nil => Task.pure(Some(v))
        case _                    => Task.pure(None)
      }
    }

    def evalExpr(expr: Expr)(implicit env: Env[Par]): Task[Expr] =
      expr.exprInstance match {
        case EPlusBody(EPlus(p1, p2)) =>
          for {
            ov1 <- evalToInt(p1.get)
            ov2 <- evalToInt(p2.get)
            retVal <- {
              (ov1, ov2) match {
                case (Some(v1), Some(v2)) => Task.pure(v1 + v2)
                case _                    => Task raiseError new Error("Invalid arithmetic.")
              }
            }
          } yield GInt(retVal)
        case _ => Task raiseError new Error("Unimplemented expression.")
      }

    def eval(mat: Match)(implicit env: Env[Par]): Task[Unit] =
      Task raiseError new Error("match is currently unimplemented.")

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
    def eval(par: Par)(implicit env: Env[Par]): Task[Unit] =
      Task.wanderUnordered(
        List(
          Task.wanderUnordered(par.sends) { send =>
            eval(send)(env)
          },
          Task.wanderUnordered(par.receives) { recv =>
            eval(recv)(env)
          },
          Task.wanderUnordered(par.news) { neu =>
            eval(neu)(env)
          },
          Task.wanderUnordered(par.evals) { deref =>
            eval(deref)(env)
          },
          Task.wanderUnordered(par.matches) { mat =>
            eval(mat)(env)
          }
        )
      ) { xs =>
        xs
      } map { xxs =>
        ()
      }
    def continue(body: Par)(implicit env: Env[Par]): Task[Unit] =
      eval(body)

    def exprEval(par: Par)(implicit env: Env[Par]): Task[Par] =
      for {
        evaledExprs <- par.exprs.toList.traverse(expr => evalExpr(expr)(env))
      } yield par.copy(exprs = evaledExprs)

    def debug(msg: String): Unit = {
      val now = java.time.format.DateTimeFormatter.ISO_INSTANT
        .format(java.time.Instant.now)
        .substring(11, 23)
      val thread = Thread.currentThread.getName
      println(s"$now [$thread]" + "\n" + msg)
    }
  }
}
