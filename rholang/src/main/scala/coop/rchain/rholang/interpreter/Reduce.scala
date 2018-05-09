package coop.rchain.rholang.interpreter

import cats.data.EitherT
import cats.implicits._
import cats.{Functor, Monad, MonadError, Eval => _}
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.{Match, MatchCase, GPrivate => _, _}
import coop.rchain.rholang.interpreter.Substitute._
import coop.rchain.rholang.interpreter.errors.{InterpreterError, ReduceError}
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.{IStore, consume => internalConsume, produce => internalProduce}
import monix.eval.Task
import coop.rchain.catscontrib._

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

// Notes: Caution, a type annotation is often needed for Env.

/** Reduce is the interface for evaluating Rholang expressions.
  *
  * @tparam M The kind of Monad used for evaluation.
  */
trait Reduce[M[_]] {

  def produce(chan: Quote, data: Seq[Par], persistent: Boolean)(implicit env: Env[Par]): M[Unit]

  def consume(binds: Seq[(BindPattern, Quote)], body: Par, persistent: Boolean)(
      implicit env: Env[Par]): M[Unit]

  def eval(par: Par)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Unit]

  // This makes sense. Run a Par in the empty environment.
  def inj(par: Par)(implicit me: MonadError[M, InterpreterError]): M[Unit] =
    for { _ <- eval(par)(Env[Par](), me) } yield ()

  /**
    * Variable "evaluation" is an environment lookup, but
    * lookup of an unbound variable should be an error.
    *
    * @param valproc The variable to be evaluated
    * @param env   The environment (possibly) containing
    *              a binding for the given variable.
    * @return If the variable has a binding (par), lift the
    *         binding into the monadic context, else signal
    *         an exception.
    */
  def eval(valproc: Var)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Par] =
    valproc.varInstance match {
      case BoundVar(level) =>
        env.get(level) match {
          case Some(par) =>
            me.pure(par)
          case None =>
            me.raiseError(ReduceError("Unbound variable: " + level + " in " + env.envMap))
        }
      case Wildcard(_) =>
        me.raiseError(ReduceError("Unbound variable: attempting to evaluate a pattern"))
      case FreeVar(_) =>
        me.raiseError(ReduceError("Unbound variable: attempting to evaluate a pattern"))
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
  def eval(channel: Channel)(implicit env: Env[Par],
                             me: MonadError[M, InterpreterError]): M[Quote] =
    channel.channelInstance match {
      case Quote(p) =>
        for { evaled <- evalExpr(p) } yield Quote(evaled)
      case ChanVar(varue) =>
        for {
          par    <- eval(varue)
          evaled <- evalExpr(par)
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
    * @param env An execution context
    * @return
    */
  def eval(send: Send)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Unit] =
    for {
      quote <- eval(send.chan.get)
      data  <- send.data.toList.traverse(x => evalExpr(x))

      subChan <- substituteQuote[M].substitute(quote).run(env)
      unbundled <- subChan.value.singleBundle() match {
                    case Some(value) =>
                      if (!value.writeFlag) {
                        me.raiseError(ReduceError("Trying to send on non-writeable channel."))
                      } else {
                        me.pure(Quote(value.body.get))
                      }
                    case None => me.pure(subChan)
                  }
      _ <- produce(unbundled, data, send.persistent)
    } yield ()

  def eval(mat: Match)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Unit] = {
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
    def firstMatch(target: Par, cases: Seq[MatchCase])(
        implicit me: MonadError[M, InterpreterError]): M[Unit] =
      cases match {
        case Nil => me.pure(())
        case singleCase +: caseRem =>
          val matchResult = SpatialMatcher
            .spatialMatch(target, singleCase.pattern.get)
            .runS(SpatialMatcher.emptyMap)
          matchResult match {
            case None => firstMatch(target, caseRem)
            case Some(freeMap) => {
              val newEnv: Env[Par] = addToEnv(env, freeMap, singleCase.freeCount)
              eval(singleCase.source.get)(newEnv, me)
            }
          }
      }
    for {
      evaledTarget <- evalExpr(mat.target.get)
      // TODO(kyle): Make the matcher accept an environment, instead of
      // substituting it.
      substTarget <- substitutePar[M].substitute(evaledTarget).run(env)
      _           <- firstMatch(substTarget, mat.cases)
    } yield ()
  }

  /**
    * Adds neu.bindCount new GPrivate from UUID's to the environment and then
    * proceeds to evaluate the body.
    *
    * @param neu
    * @param env
    * @return
    */
  def eval(neu: New)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Unit] = {
    def alloc(level: Int)(implicit env: Env[Par]): M[Env[Par]] =
      me pure {
        (env /: (0 until level).toList) { (_env, _) =>
          val addr: Par = GPrivate()
          _env.put(addr)
        }
      }
    for {
      _env <- alloc(neu.bindCount)
      _    <- eval(neu.p.get)(_env, me)
    } yield ()
  }

  private[this] def unbundleReceive(
      rb: ReceiveBind)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Quote] =
    for {
      quote <- eval(rb.source.get)
      subst <- substituteQuote[M].substitute(quote).run(env)
      // Check if we try to read from bundled channel
      unbndl <- subst.quote.get.singleBundle() match {
                 case Some(value) =>
                   if (!value.readFlag) {
                     me.raiseError(ReduceError("Trying to read from non-readable channel."))
                   } else {
                     me.pure(Quote(value.body.get))
                   }
                 case None =>
                   me.pure(subst)
               }
    } yield unbndl

  /**
    * Eval is well-defined on channel variables provided
    * a binding exists for the variable. It simply gets the
    * quoted process and calls eval.
    */
  def eval(drop: Eval)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Unit] =
    for {
      quote <- eval(drop.channel.get)
      _     <- eval(quote.value)
    } yield ()

  def eval(receive: Receive)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Unit] =
    for {
      binds <- receive.binds.toList
                .traverse(rb =>
                  unbundleReceive(rb).map(q =>
                    (BindPattern(rb.patterns, rb.remainder, rb.freeCount), q)))
      // TODO: Allow for the environment to be stored with the body in the Tuplespace
      substBody <- substitutePar[M].substitute(receive.body.get).run(env.shift(receive.bindCount))
      _         <- consume(binds, substBody, receive.persistent)
    } yield ()

  def eval(bundle: Bundle)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Unit] =
    eval(bundle.body.get)

  /**
    * Continue is straightforward in this case, it just calls eval.
    */
  def continue(body: Par)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Unit] =
    eval(body)

  def evalExprToPar(expr: Expr)(implicit env: Env[Par],
                                me: MonadError[M, InterpreterError]): M[Par] =
    expr.exprInstance match {
      case EVarBody(EVar(v)) =>
        for {
          p       <- eval(v.get)
          evaledP <- evalExpr(p)
        } yield evaledP
      case EMethodBody(EMethod(method, target, arguments, _, _)) => {
        val methodLookup = methodTable(method)
        for {
          evaledTarget <- evalExpr(target.get)
          evaledArgs   <- arguments.toList.traverse(expr => evalExpr(expr))
          resultPar <- methodLookup match {
                        case None    => me.raiseError(ReduceError("Unimplemented method: " + method))
                        case Some(f) => f(target.get, evaledArgs)(env)
                      }
        } yield resultPar
      }
      case _ => evalExprToExpr(expr).map(e => (fromExpr(e)(identity)))
    }

  def evalExprToExpr(expr: Expr)(implicit env: Env[Par],
                                 me: MonadError[M, InterpreterError]): M[Expr] = {
    implicit val parSubstitute = Substitute.substitutePar[M]
    def relop(p1: Par,
              p2: Par,
              relopb: (Boolean, Boolean) => Boolean,
              relopi: (Int, Int) => Boolean,
              relops: (String, String) => Boolean): M[Expr] =
      for {
        v1 <- evalSingleExpr(p1)
        v2 <- evalSingleExpr(p2)
        result <- (v1.exprInstance, v2.exprInstance) match {
                   case (GBool(b1), GBool(b2))     => me.pure(GBool(relopb(b1, b2)))
                   case (GInt(i1), GInt(i2))       => me.pure(GBool(relopi(i1, i2)))
                   case (GString(s1), GString(s2)) => me.pure(GBool(relops(s1, s2)))
                   case _                          => me.raiseError(ReduceError("Unexpected compare: " + v1 + " vs. " + v2))
                 }
      } yield result

    expr.exprInstance match {
      case x: GBool   => me.pure[Expr](x)
      case x: GInt    => me.pure[Expr](x)
      case x: GString => me.pure[Expr](x)
      case x: GUri    => me.pure[Expr](x)
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
      case ELteBody(ELte(p1, p2)) => relop(p1.get, p2.get, (_ <= _), (_ <= _), (_ <= _))
      case EGtBody(EGt(p1, p2))   => relop(p1.get, p2.get, (_ > _), (_ > _), (_ > _))
      case EGteBody(EGte(p1, p2)) => relop(p1.get, p2.get, (_ >= _), (_ >= _), (_ >= _))
      case EEqBody(EEq(p1, p2)) =>
        for {
          v1 <- evalExpr(p1.get)
          v2 <- evalExpr(p2.get)
          // TODO: build an equality operator that takes in an environment.
          sv1 <- substitute(v1).run(env)
          sv2 <- substitute(v2).run(env)
        } yield GBool(sv1 == sv2)
      case ENeqBody(ENeq(p1, p2)) =>
        for {
          v1  <- evalExpr(p1.get)
          v2  <- evalExpr(p2.get)
          sv1 <- substitute(v1).run(env)
          sv2 <- substitute(v2).run(env)
        } yield GBool(sv1 != sv2)
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
      case EVarBody(EVar(v)) =>
        for {
          p       <- eval(v.get)
          exprVal <- evalSingleExpr(p)
        } yield exprVal
      case EListBody(el) => {
        for {
          evaledPs  <- el.ps.toList.traverse(expr => evalExpr(expr))
          updatedPs = evaledPs.map(updateLocallyFree)
        } yield updateLocallyFree(EList(updatedPs, el.locallyFree, el.connectiveUsed))
      }
      case EMethodBody(EMethod(method, target, arguments, _, _)) => {
        val methodLookup = methodTable(method)
        for {
          evaledTarget <- evalExpr(target.get)
          evaledArgs   <- arguments.toList.traverse(expr => evalExpr(expr))
          resultPar <- methodLookup match {
                        case None    => me raiseError ReduceError("Unimplemented method: " + method)
                        case Some(f) => f(target.get, evaledArgs)(env)
                      }
          resultExpr <- evalSingleExpr(resultPar)
        } yield resultExpr
      }
      case _ => me.raiseError(ReduceError("Unimplemented expression: " + expr))
    }
  }

  private[this] def nth(
      implicit err: MonadError[M, InterpreterError]): (Par, Seq[Par]) => Env[Par] => M[Par] = {
    def localNth(ps: Seq[Par], nth: Int): M[Par] =
      if (ps.isDefinedAt(nth)) {
        Monad[M].pure(ps(nth))
      } else {
        err.raiseError(ReduceError("Error: index out of bound: " + nth))
      }

    (p: Par, args: Seq[Par]) => (env: Env[Par]) =>
      {
        implicit val environment = env
        if (args.length != 1) {
          err.raiseError(ReduceError("Error: nth expects 1 argument"))
        } else {
          for {
            nth <- evalToInt(args(0))
            v   <- evalSingleExpr(p)
            result <- v.exprInstance match {
                       case EListBody(EList(ps, _, _, _)) => localNth(ps, nth)
                       case ETupleBody(ETuple(ps, _, _))  => localNth(ps, nth)
                       case _ =>
                         err.raiseError(
                           ReduceError(
                             "Error: nth applied to something that wasn't a list or tuple."))
                     }
          } yield result
        }
      }
  }

  def methodTable(method: String)(
      implicit ev: MonadError[M, InterpreterError]): Option[(Par, Seq[Par]) => Env[Par] => M[Par]] =
    method match {
      case "nth" => Some(nth)
      case _     => None
    }

  def evalSingleExpr(p: Par)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Expr] =
    if (!p.sends.isEmpty || !p.receives.isEmpty || !p.evals.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
      me.raiseError(
        ReduceError("Error: parallel or non expression found where expression expected."))
    else
      p.exprs match {
        case (e: Expr) +: Nil => evalExprToExpr(e)
        case _ =>
          me.raiseError(ReduceError("Error: Multiple expressions given."))
      }

  def evalToInt(p: Par)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Int] =
    if (!p.sends.isEmpty || !p.receives.isEmpty || !p.evals.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
      me.raiseError(
        ReduceError("Error: parallel or non expression found where expression expected."))
    else
      p.exprs match {
        case Expr(GInt(v)) +: Nil => Monad[M].pure(v)
        case Expr(EVarBody(EVar(v))) +: Nil =>
          for {
            p      <- eval(v.get)
            intVal <- evalToInt(p)
          } yield intVal
        case (e: Expr) +: Nil =>
          for {
            evaled <- evalExprToExpr(e)
            result <- evaled.exprInstance match {
                       case GInt(v) => Monad[M].pure(v)
                       case _ =>
                         me.raiseError(ReduceError("Error: expression didn't evaluate to integer."))
                     }
          } yield result
        case _ =>
          me.raiseError(ReduceError("Error: Integer expected, or unimplemented expression."))
      }

  def evalToBool(p: Par)(implicit env: Env[Par], me: MonadError[M, InterpreterError]): M[Boolean] =
    if (!p.sends.isEmpty || !p.receives.isEmpty || !p.evals.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
      me.raiseError(
        ReduceError("Error: parallel or non expression found where expression expected."))
    else
      p.exprs match {
        case Expr(GBool(b)) +: Nil => Monad[M].pure(b)
        case Expr(EVarBody(EVar(v))) +: Nil =>
          for {
            p       <- eval(v.get)
            boolVal <- evalToBool(p)
          } yield boolVal
        case (e: Expr) +: Nil =>
          for {
            evaled <- evalExprToExpr(e)
            result <- evaled.exprInstance match {
                       case GBool(b) => Monad[M].pure(b)
                       case _ =>
                         me.raiseError(ReduceError("Error: expression didn't evaluate to boolean."))
                     }
          } yield result
        case _ =>
          me.raiseError(ReduceError("Error: Multiple expressions given."))
      }

  def updateLocallyFree(par: Par): Par = {
    val resultLocallyFree =
      par.sends.foldLeft(BitSet())((acc, send) => acc | send.locallyFree) |
        par.receives.foldLeft(BitSet())((acc, receive) => acc | receive.locallyFree) |
        par.evals.foldLeft(BitSet())((acc, eval) => acc | EvalLocallyFree.locallyFree(eval)) |
        par.news.foldLeft(BitSet())((acc, newProc) => acc | newProc.locallyFree) |
        par.exprs.foldLeft(BitSet())((acc, expr) => acc | ExprLocallyFree.locallyFree(expr)) |
        par.matches.foldLeft(BitSet())((acc, matchProc) => acc | matchProc.locallyFree)
    par.copy(locallyFree = resultLocallyFree)
  }

  def updateLocallyFree(elist: EList): EList = {
    val resultLocallyFree = elist.ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
    elist.copy(locallyFree = resultLocallyFree)
  }

  /**
    * Evaluate any top level expressions in @param Par .
    */
  def evalExpr(par: Par)(implicit env: Env[Par], ev: MonadError[M, InterpreterError]): M[Par] =
    for {
      evaledExprs <- par.exprs.toList.traverse(expr => evalExprToPar(expr))
      result = evaledExprs.foldLeft(par.copy(exprs = Vector())) { (acc, newPar) =>
        acc ++ newPar
      }
    } yield result

}

object Reduce {

  class DebruijnInterpreter(
      tupleSpace: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
      dispatcher: => Dispatch[Task, Seq[Channel], TaggedContinuation])
      extends Reduce[Task] {

    type Cont[Data, Body] = (Body, Env[Data])

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
        implicit env: Env[Par]): Task[Unit] =
      // TODO: Handle the environment in the store
      for {
        substData <- data.toList
                      .traverse(substitutePar[Task].substitute(_).map(p => Channel(Quote(p))))
                      .run(env)
        res <- internalProduce(tupleSpace, Channel(chan), substData, persist = persistent) match {
                case Some((continuation, dataList)) =>
                  if (persistent) {
                    Task
                      .gather {
                        Seq(dispatcher.dispatch(continuation, dataList),
                            produce(chan, data, persistent)(env))
                      }
                      .map(_ => ())
                  } else {
                    dispatcher.dispatch(continuation, dataList)
                  }
                case None => Task.unit
              }
      } yield res

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
    def consume(binds: Seq[(BindPattern, Quote)], body: Par, persistent: Boolean)(
        implicit env: Env[Par]): Task[Unit] =
      binds match {
        case Nil => Task.raiseError(ReduceError("Error: empty binds"))
        case _ =>
          val (patterns: Seq[BindPattern], sources: Seq[Quote]) =
            binds.unzip
          internalConsume(tupleSpace,
                          sources.map(q => Channel(q)).toList,
                          patterns.toList,
                          TaggedContinuation(ParBody(body)),
                          persist = persistent) match {
            case Some((continuation, dataList)) =>
              dispatcher.dispatch(continuation, dataList)
              if (persistent) {
                Task
                  .gather {
                    Seq(dispatcher.dispatch(continuation, dataList),
                        consume(binds, body, persistent)(env))
                  }
                  .map(_ => ())
              } else {
                dispatcher.dispatch(continuation, dataList)
              }
            case None => Task.unit
          }
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
    override def eval(par: Par)(implicit env: Env[Par],
                                me: MonadError[Task, InterpreterError]): Task[Unit] =
      Task
        .gatherUnordered(
          Seq(
            Task
              .wanderUnordered(par.sends) { send =>
                eval(send)
              },
            Task.wanderUnordered(par.receives) { recv =>
              eval(recv)
            },
            Task.wanderUnordered(par.news) { neu =>
              eval(neu)
            },
            Task.wanderUnordered(par.evals) { deref =>
              eval(deref)
            },
            Task.wanderUnordered(par.matches) { mat =>
              eval(mat)
            },
            Task.wanderUnordered(par.bundles) { bundle =>
              eval(bundle)
            },
            Task.wanderUnordered(par.exprs.filter { expr =>
              expr.exprInstance match {
                case _: EVarBody    => true
                case _: EMethodBody => true
                case _              => false
              }
            }) { expr =>
              expr.exprInstance match {
                case EVarBody(EVar(v)) =>
                  for {
                    varref <- eval(v.get)
                    _      <- eval(varref)
                  } yield ()
                case e: EMethodBody =>
                  for {
                    p <- evalExprToPar(Expr(e))
                    _ <- eval(p)
                  } yield ()
                case _ => Task.unit
              }
            }
          )
        )
        .map(_ => ())

    def debug(msg: String): Unit = {
      val now = java.time.format.DateTimeFormatter.ISO_INSTANT
        .format(java.time.Instant.now)
        .substring(11, 23)
      val thread = Thread.currentThread.getName
      println(s"$now [$thread]" + "\n" + msg)
    }

  }
}
