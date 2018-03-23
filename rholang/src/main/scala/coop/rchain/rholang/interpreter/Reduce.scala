package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Match
import coop.rchain.models.MatchCase
import coop.rchain.models.{GPrivate => _, _}
import Substitute._
import Env._
import implicits._
import cats.{Eval => _, _}
import cats.data._
import cats.implicits._
import monix.eval.{MVar, Task}

import scala.annotation.tailrec
import storage.implicits._
import coop.rchain.storage.{IStore, consume => internalConsume, produce => internalProduce}

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

  class DebruijnInterpreter(tupleSpace: IStore[Channel, List[Channel], List[Quote], Par])
      extends Reduce[Task, Quote, Par, Channel, Par] {

    // This makes sense. Run a Par in the empty environment.
    def inj(par: Par): Task[Unit] =
      for { _ <- eval(par)(Env[Par]()) } yield ()

    /**
      * Materialize a send in the store, optionally returning the matched continuation.
      *
      * @param chan  The channel on which data is being sent.
      * @param data  The par objects holding the processes being sent.
      * @param persistent  True if the write should remain in the tuplespace indefinitely.
      * @param env  An environment marking the execution context.
      * @return  An optional continuation resulting from a match in the tuplespace.
      */
    def produce(chan: Quote, data: Seq[Par], persistent: Boolean)(
        implicit env: Env[Par]): Task[Option[Cont[Par, Par]]] = {
      // TODO: Handle the environment in the store
      val substData: List[Quote] = data.toList.map(p => Quote(substitute(p)(env)))
      internalProduce(tupleSpace, Channel(chan), substData, persist = persistent) match {
        case Some((body, dataList)) =>
          val newEnv: Env[Par] =
            Env.makeEnv(dataList.flatMap(identity).map({ case Quote(p) => p }): _*)
          Task.pure(Some((body, newEnv)))
        case None => Task.pure(None)
      }
    }

    /**
      * Materialize a send in the store, optionally returning the matched continuation.
      *
      * @param binds  A Seq of pattern, channel pairs. Each pattern is a Seq[Channel].
      *               The Seq is for arity matching, and each term in the Seq is a name pattern.
      * @param body  A Par object which will be run in the envirnoment resulting from the match.
      * @param env  The current environment, to which the matches will be added before resuming
      *             execution in body
      * @return  An optional continuation resulting from a match. The body of the continuation
      *          will be @param body if the continuation is not None.
      */
    def consume(binds: Seq[(Seq[Channel], Quote)], body: Par, persistent: Boolean)(
        implicit env: Env[Par]): Task[Option[Cont[Par, Par]]] = {
      // TODO: Allow for the environment to be stored with the body in the Tuplespace
      val substBody = substitute(body)(env)
      binds match {
        case Nil => Task raiseError new Error("Error: empty binds")
        case _ =>
          val (patterns: List[List[Channel]], channels: List[Quote]) = binds.unzip
          internalConsume(tupleSpace,
                          channels.map(q => Channel(q)),
                          patterns,
                          substBody,
                          persist = persistent) match {
            case Some((continuation, dataList)) =>
              val newEnv: Env[Par] =
                Env.makeEnv(dataList.flatten.map({ case Quote(p) => p }): _*)
              Task.pure(Some((continuation, newEnv)))
            case None => Task.pure(None)
          }
      }
    }

    /**
      * Variable "evaluation" is an environment lookup, but
      * lookup of an unbound variable should be an error.
      *
      * @param varue The variable to be evaluated
      * @param env   The environment (possibly) containing
      *              a binding for the given variable.
      * @return If the variable has a binding (par), lift the
      *         binding into the monadic context, else signal
      *         an exception.
      */
    def eval(valproc: Var)(implicit env: Env[Par]): Task[Par] =
      valproc.varInstance match {
        case BoundVar(level) =>
          env.get(level) match {
            case Some(par) =>
              Task.pure(par)
            case None =>
              Task raiseError new IllegalStateException("Unbound variable")
          }
        case Wildcard(_) =>
          Task raiseError new IllegalStateException(
            "Unbound variable: attempting to evaluate a pattern")
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
      * In either case the top level expressions of the quoted process are evaluated
      * when the channel is evaluated.
      *
      * @param channel The channel to be evaluated
      * @param env An environment that (possibly) has
      *            a binding for channel
      * @return A quoted process or "channel value"
      */
    def eval(channel: Channel)(implicit env: Env[Par]): Task[Quote] =
      channel.channelInstance match {
        case Quote(p) =>
          for { evaled <- evalExpr(p)(env) } yield Quote(evaled)
        case ChanVar(varue) =>
          for {
            par    <- eval(varue)(env)
            evaled <- evalExpr(par)(env)
          } yield Quote(evaled)
      }

    /** Algorithm as follows:
      *
      * 1. Fully evaluate the channel in given environment.
      *    (See eval(Channel) to see all that entails)
      * 2. Substitute any variable references in the channel so that it can be
      *    correctly used as a key in the tuple space.
      * 3. Evaluate any top level expressions in the data being sent.
      * 4. Call produce
      * 5. If produce returned a continuation, evaluate it.
      * @param send An output process
      * @param env0 An execution context
      * @return
      */
    def eval(send: Send)(implicit env: Env[Par]): Task[Unit] = {
      println("sending: " + send)
      for {
        quote           <- eval(send.chan.get)
        data            <- send.data.toList.traverse(x => evalExpr(x)(env))
        subChan: Quote  = substitute(quote)
        _               = println("quote is: " + quote)
        _               = println("subChan is: " + subChan)
        _               = println("data is: " + data)
        optContinuation <- produce(subChan, data, send.persistent)

        _ <- optContinuation match {
              case Some((body: Par, newEnv: Env[Par])) =>
                continue(body)(newEnv)
              case None => Task.unit
            }
      } yield ()
    }

    def eval(receive: Receive)(implicit env: Env[Par]): Task[Unit] =
      for {
        binds <- receive.binds.toList
                  .traverse((rb: ReceiveBind) =>
                    eval(rb.source.get).map(quote => (rb.patterns, substitute(quote))))
        optContinuation <- consume(binds, receive.body.get, receive.persistent)
        _ <- optContinuation match {
              case Some((body: Par, newEnv: Env[Par])) =>
                continue(body)(newEnv)
              case None => Task.unit
            }
      } yield ()

    /**
      * Eval is well-defined on channel variables provided
      * a binding exists for the variable. It simply gets the
      * quoted process and calls eval.
      */
    def eval(drop: Eval)(implicit env: Env[Par]): Task[Unit] =
      for {
        quote <- eval(drop.channel.get)
        _     <- eval(quote.value)
      } yield ()

    /**
      * Adds neu.bindCount new GPrivate from UUID's to the environment and then
      * proceeds to evaluate the body.
      *
      * @param neu
      * @param env
      * @return
      */
    def eval(neu: New)(implicit env: Env[Par]): Task[Unit] = {
      def alloc(level: Int)(implicit env: Env[Par]): Task[Env[Par]] =
        Task now {
          (env /: (0 until level).toList) { (_env, _) =>
            val addr: Par = GPrivate()
            _env.put(addr)
          }
        }
      for {
        _env <- alloc(neu.bindCount)
        _    <- eval(neu.p.get)(_env)
      } yield ()
    }

    def evalToInt(p: Par)(implicit env: Env[Par]): Task[Int] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.evals.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
        Task raiseError new Error(
          "Error: parallel or non expression found where expression expected.")
      else
        p.exprs match {
          case Expr(GInt(v)) +: Nil => Task.pure(v)
          case Expr(EVarBody(EVar(v))) +: Nil =>
            for {
              p      <- eval(v.get)
              intVal <- evalToInt(p)
            } yield intVal
          case (e: Expr) +: Nil =>
            for {
              evaled <- evalExpr(e)
              result <- evaled.exprInstance match {
                         case GInt(v) => Task.pure(v)
                         case _ =>
                           Task raiseError new Error(
                             "Error: expression didn't evaluate to integer.")
                       }
            } yield result
          case _ =>
            Task raiseError new Error("Error: Integer expected, or unimplemented expression.")
        }

    def evalToBool(p: Par)(implicit env: Env[Par]): Task[Boolean] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.evals.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
        Task raiseError new Error(
          "Error: parallel or non expression found where expression expected.")
      else
        p.exprs match {
          case Expr(GBool(b)) +: Nil => Task.pure(b)
          case Expr(EVarBody(EVar(v))) +: Nil =>
            for {
              p       <- eval(v.get)
              boolVal <- evalToBool(p)
            } yield boolVal
          case (e: Expr) +: Nil =>
            for {
              evaled <- evalExpr(e)
              result <- evaled.exprInstance match {
                         case GBool(b) => Task.pure(b)
                         case _ =>
                           Task raiseError new Error(
                             "Error: expression didn't evaluate to boolean.")
                       }
            } yield result
          case _ =>
            Task raiseError new Error("Error: Multiple expressions given.")
        }

    def evalSingleExpr(p: Par)(implicit env: Env[Par]): Task[Expr] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.evals.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
        Task raiseError new Error(
          "Error: parallel or non expression found where expression expected.")
      else
        p.exprs match {
          case Expr(EVarBody(EVar(v))) +: Nil =>
            for {
              p       <- eval(v.get)
              exprVal <- evalSingleExpr(p)
            } yield exprVal
          case (e: Expr) +: Nil => evalExpr(e)
          case _ =>
            Task raiseError new Error("Error: Multiple expressions given.")
        }

    def evalExpr(expr: Expr)(implicit env: Env[Par]): Task[Expr] = {
      def relop(p1: Par,
                p2: Par,
                relopb: (Boolean, Boolean) => Boolean,
                relopi: (Int, Int) => Boolean,
                relops: (String, String) => Boolean): Task[Expr] =
        for {
          v1 <- evalSingleExpr(p1)
          v2 <- evalSingleExpr(p2)
          result <- (v1.exprInstance, v2.exprInstance) match {
                     case (GBool(b1), GBool(b2))     => Task.pure(GBool(relopb(b1, b2)))
                     case (GInt(i1), GInt(i2))       => Task.pure(GBool(relopi(i1, i2)))
                     case (GString(s1), GString(s2)) => Task.pure(GBool(relops(s1, s2)))
                     case _                          => Task raiseError new Error("Unexpected compare: " + v1 + " vs. " + v2)
                   }
        } yield result

      expr.exprInstance match {
        case x: GBool   => Task.pure[Expr](x)
        case x: GInt    => Task.pure[Expr](x)
        case x: GString => Task.pure[Expr](x)
        case x: GUri    => Task.pure[Expr](x)
        case ENotBody(ENot(p)) =>
          for {
            b <- evalToBool(p.get)
          } yield GBool(!b)
        case ENegBody(ENeg(p)) =>
          for {
            v <- evalToInt(p.get)
          } yield GInt(-v)
        case EMultBody(EMult(p1, p2)) =>
          for {
            v1 <- evalToInt(p1.get)
            v2 <- evalToInt(p2.get)
          } yield GInt(v1 * v2)
        case EDivBody(EDiv(p1, p2)) =>
          for {
            v1 <- evalToInt(p1.get)
            v2 <- evalToInt(p2.get)
          } yield GInt(v1 / v2)
        case EPlusBody(EPlus(p1, p2)) =>
          for {
            v1 <- evalToInt(p1.get)
            v2 <- evalToInt(p2.get)
          } yield GInt(v1 + v2)
        case EMinusBody(EMinus(p1, p2)) =>
          for {
            v1 <- evalToInt(p1.get)
            v2 <- evalToInt(p2.get)
          } yield GInt(v1 - v2)
        case ELtBody(ELt(p1, p2))   => relop(p1.get, p2.get, (_ < _), (_ < _), (_ < _))
        case ELteBody(ELte(p1, p2)) => relop(p1.get, p2.get, (_ <= _), (_ <= _), (_ < _))
        case EGtBody(EGt(p1, p2))   => relop(p1.get, p2.get, (_ > _), (_ > _), (_ > _))
        case EGteBody(EGte(p1, p2)) => relop(p1.get, p2.get, (_ >= _), (_ >= _), (_ >= _))
        case EEqBody(EEq(p1, p2)) =>
          for {
            v1 <- evalSingleExpr(p1.get)
            v2 <- evalSingleExpr(p2.get)
          } yield GBool(v1 == v2)
        case ENeqBody(ENeq(p1, p2)) =>
          for {
            v1 <- evalSingleExpr(p1.get)
            v2 <- evalSingleExpr(p2.get)
          } yield GBool(v1 != v2)
        case EAndBody(EAnd(p1, p2)) =>
          for {
            b1 <- evalToBool(p1.get)
            b2 <- evalToBool(p2.get)
          } yield GBool(b1 && b2)
        case EOrBody(EOr(p1, p2)) =>
          for {
            b1 <- evalToBool(p1.get)
            b2 <- evalToBool(p2.get)
          } yield GBool(b1 || b2)
        case EVarBody(ev) => Task.pure[Expr](ev)
        case _            => Task raiseError new Error("Unimplemented expression.")
      }
    }

    def eval(mat: Match)(implicit env: Env[Par]): Task[Unit] = {
      def addToEnv(env: Env[Par], freeMap: Map[Int, Par], freeCount: Int): Env[Par] =
        Range(0, freeCount).foldLeft(env)(
          (acc, e) =>
            acc.put(
              freeMap.get(e) match {
                case Some(p) => p
                case None    => Par()
              }
          )
        )
      @tailrec
      def firstMatch(target: Par, cases: Seq[MatchCase]): Task[Unit] =
        cases match {
          case Nil => Task.unit
          case singleCase +: caseRem =>
            val matchResult = SpatialMatcher
              .spatialMatch(target, singleCase.pattern.get)
              .runS(SpatialMatcher.emptyMap)
            matchResult match {
              case None => firstMatch(target, caseRem)
              case Some(freeMap) => {
                val newEnv = addToEnv(env, freeMap, singleCase.pattern.get.freeCount)
                eval(singleCase.source.get)(newEnv)
              }
            }
        }
      for {
        evaledTarget <- evalExpr(mat.target.get)
        // TODO(kyle): Make the matcher accept an environment, instead of
        // substituting it.
        _ <- firstMatch(substitute(evaledTarget)(env), mat.cases)
      } yield ()
    }

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
          },
          Task.wanderUnordered(
            par.exprs
              .map(expr =>
                expr.exprInstance match {
                  case EVarBody(EVar(v)) => Some(v)
                  case _                 => None
              })
              .flatten) { varproc =>
            for {
              varref <- eval(varproc.get)
              _      <- eval(varref)
            } yield ()
          }
        )
      ) { xs =>
        xs
      } map { xxs =>
        ()
      }

    /**
      * Continue is straightforward in this case, it just calls eval.
      */
    def continue(body: Par)(implicit env: Env[Par]): Task[Unit] =
      eval(body)

    /**
      * Evaluate any top level expressions in @param Par .
      */
    def evalExpr(par: Par)(implicit env: Env[Par]): Task[Par] =
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

  def makeInterpreter(
      tupleSpace: IStore[Channel, List[Channel], List[Quote], Par]): DebruijnInterpreter =
    new DebruijnInterpreter(tupleSpace)
}
