package coop.rchain.rholang.interpreter

import cats.implicits._
import cats.{Applicative, Parallel, Eval => _}
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.implicits._
import coop.rchain.models.{Match, MatchCase, GPrivate => _, _}
import coop.rchain.rholang.interpreter.Substitute._
import coop.rchain.rholang.interpreter.errors.{InterpreterErrorsM, ReduceError, _}
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.Serialize
import coop.rchain.rspace.pure.PureRSpace

import scala.collection.immutable.BitSet
import scala.util.Try

// Notes: Caution, a type annotation is often needed for Env.

/** Reduce is the interface for evaluating Rholang expressions.
  *
  * @tparam M The kind of Monad used for evaluation.
  */
trait Reduce[M[_]] {

  def produce(chan: Quote, data: Seq[Par], persistent: Boolean)(implicit env: Env[Par]): M[Unit]

  def consume(binds: Seq[(BindPattern, Quote)], body: Par, persistent: Boolean)(
      implicit env: Env[Par]): M[Unit]

  def eval(par: Par)(implicit env: Env[Par]): M[Unit]

  def inj(par: Par): M[Unit]

  /**
    * Evaluate any top level expressions in @param Par .
    */
  def evalExpr(par: Par)(implicit env: Env[Par]): M[Par]

  def evalExprToPar(expr: Expr)(implicit env: Env[Par]): M[Par]
}

object Reduce {

  class DebruijnInterpreter[M[_]: InterpreterErrorsM: Capture, F[_]](
      tupleSpace: PureRSpace[M, Channel, BindPattern, Seq[Channel], TaggedContinuation],
      dispatcher: => Dispatch[M, Seq[Channel], TaggedContinuation])(
      implicit parallel: cats.Parallel[M, F])
      extends Reduce[M] {

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
    override def produce(chan: Quote, data: Seq[Par], persistent: Boolean)(
        implicit env: Env[Par]): M[Unit] = {
      // TODO: Handle the environment in the store
      def go(res: Option[(TaggedContinuation, Seq[Seq[Channel]])]) =
        res match {
          case Some((continuation, dataList)) =>
            if (persistent) {
              Parallel.parSequence_[List, M, F, Unit](
                List(dispatcher.dispatch(continuation, dataList), produce(chan, data, persistent)))
            } else {
              dispatcher.dispatch(continuation, dataList)
            }
          case None =>
            Applicative[M].pure(())
        }

      for {
        substData <- data.toList.traverse(
                      substitutePar[M].substitute(_).map(p => Channel(Quote(p))))
        res <- tupleSpace.produce(Channel(chan), substData, persist = persistent)
        _   <- go(res)
      } yield ()
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
    override def consume(binds: Seq[(BindPattern, Quote)], body: Par, persistent: Boolean)(
        implicit env: Env[Par]): M[Unit] =
      binds match {
        case Nil => interpreterErrorM[M].raiseError(ReduceError("Error: empty binds"))
        case _ =>
          val (patterns: Seq[BindPattern], sources: Seq[Quote]) = binds.unzip
          tupleSpace
            .consume(sources.map(q => Channel(q)).toList,
                     patterns.toList,
                     TaggedContinuation(ParBody(body)),
                     persist = persistent)
            .flatMap {
              case Some((continuation, dataList)) =>
                dispatcher.dispatch(continuation, dataList)
                if (persistent) {
                  List(dispatcher.dispatch(continuation, dataList),
                       consume(binds, body, persistent)).parSequence
                    .map(_ => ())
                } else {
                  dispatcher.dispatch(continuation, dataList)
                }
              case None => Applicative[M].pure(())
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
      * @return
      */
    override def eval(par: Par)(implicit env: Env[Par]): M[Unit] =
      List(
        Parallel.parTraverse(par.sends.toList)(send => eval(send)),
        Parallel.parTraverse(par.receives.toList)(recv => eval(recv)),
        Parallel.parTraverse(par.news.toList)(neu => eval(neu)),
        Parallel.parTraverse(par.matches.toList)(mat => eval(mat)),
        Parallel.parTraverse(par.bundles.toList)(bundle => eval(bundle)),
        Parallel.parTraverse(par.exprs.filter { expr =>
          expr.exprInstance match {
            case _: EVarBody    => true
            case _: EEvalBody   => true
            case _: EMethodBody => true
            case _              => false
          }
        }.toList)(expr =>
          expr.exprInstance match {
            case EVarBody(EVar(v)) =>
              for {
                varref <- eval(v.get)
                _      <- eval(varref)
              } yield ()
            case e: EEvalBody =>
              for {
                p <- evalExprToPar(Expr(e))
                _ <- eval(p)
              } yield ()
            case e: EMethodBody =>
              for {
                p <- evalExprToPar(Expr(e))
                _ <- eval(p)
              } yield ()
            case _ => Applicative[M].pure(())
        })
      ).parSequence.map(_ => ())

    override def inj(par: Par): M[Unit] =
      for { _ <- eval(par)(Env[Par]()) } yield ()

    def debug(msg: String): Unit = {
      val now = java.time.format.DateTimeFormatter.ISO_INSTANT
        .format(java.time.Instant.now)
        .substring(11, 23)
      val thread = Thread.currentThread.getName
      println(s"$now [$thread]" + "\n" + msg)
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
    def eval(send: Send)(implicit env: Env[Par]): M[Unit] =
      for {
        quote <- eval(send.chan.get)
        data  <- send.data.toList.traverse(x => evalExpr(x))

        subChan <- substituteQuote[M].substitute(quote)
        unbundled <- subChan.value.singleBundle() match {
                      case Some(value) =>
                        if (!value.writeFlag) {
                          interpreterErrorM[M].raiseError(
                            ReduceError("Trying to send on non-writeable channel."))
                        } else {
                          interpreterErrorM[M].pure(Quote(value.body.get))
                        }
                      case None => Applicative[M].pure(subChan)
                    }
        _ <- produce(unbundled, data, send.persistent)
      } yield ()

    def eval(receive: Receive)(implicit env: Env[Par]): M[Unit] =
      for {
        binds <- receive.binds.toList
                  .traverse(rb =>
                    unbundleReceive(rb).map(q =>
                      (BindPattern(rb.patterns, rb.remainder, rb.freeCount), q)))
        // TODO: Allow for the environment to be stored with the body in the Tuplespace
        substBody <- substitutePar[M].substitute(receive.body.get)(env.shift(receive.bindCount))
        _         <- consume(binds, substBody, receive.persistent)
      } yield ()

    /**
      * Variable "evaluation" is an environment lookup, but
      * lookup of an unbound variable should be an error.
      *
      * @param valproc The variable to be evaluated
      * @param env  provides the environment (possibly) containing a binding for the given variable.
      * @return If the variable has a binding (par), lift the
      *                  binding into the monadic context, else signal
      *                  an exception.
      *
      */
    def eval(valproc: Var)(implicit env: Env[Par]): M[Par] =
      valproc.varInstance match {
        case BoundVar(level) =>
          env.get(level) match {
            case Some(par) => interpreterErrorM[M].pure(par)
            case None =>
              interpreterErrorM[M].raiseError(
                ReduceError("Unbound variable: " + level + " in " + env.envMap))
          }
        case Wildcard(_) =>
          interpreterErrorM[M].raiseError(
            ReduceError("Unbound variable: attempting to evaluate a pattern"))
        case FreeVar(_) =>
          interpreterErrorM[M].raiseError(
            ReduceError("Unbound variable: attempting to evaluate a pattern"))
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
    def eval(channel: Channel)(implicit env: Env[Par]): M[Quote] =
      channel.channelInstance match {
        case Quote(p) =>
          for { evaled <- evalExpr(p) } yield Quote(evaled)
        case ChanVar(varue) =>
          for {
            par    <- eval(varue)
            evaled <- evalExpr(par)
          } yield Quote(evaled)
      }

    def eval(mat: Match)(implicit env: Env[Par]): M[Unit] = {
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

      @annotation.tailrec
      def firstMatch(target: Par, cases: Seq[MatchCase])(implicit env: Env[Par]): M[Unit] =
        cases match {
          case Nil => Applicative[M].pure(())
          case singleCase +: caseRem =>
            val matchResult = SpatialMatcher
              .spatialMatch(target, singleCase.pattern.get)
              .runS(SpatialMatcher.emptyMap)
            matchResult match {
              case None => firstMatch(target, caseRem)
              case Some(freeMap) => {
                val newEnv: Env[Par] = addToEnv(env, freeMap, singleCase.freeCount)
                eval(singleCase.source.get)(newEnv)
              }
            }
        }

      for {
        evaledTarget <- evalExpr(mat.target.get)
        // TODO(kyle): Make the matcher accept an environment, instead of
        // substituting it.
        substTarget <- substitutePar[M].substitute(evaledTarget)(env)
        _           <- firstMatch(substTarget, mat.cases)
      } yield ()
    }

    /**
      * Adds neu.bindCount new GPrivate from UUID's to the environment and then
      * proceeds to evaluate the body.
      *
      * @param neu
      * @return
      */
    def eval(neu: New)(implicit env: Env[Par]): M[Unit] = {
      def alloc(level: Int): Env[Par] =
        (env /: (0 until level).toList) { (_env, _) =>
          val addr: Par = GPrivate()
          _env.put(addr)
        }

      eval(neu.p.get)(alloc(neu.bindCount))
    }

    private[this] def unbundleReceive(rb: ReceiveBind)(implicit env: Env[Par]): M[Quote] =
      for {
        quote <- eval(rb.source.get)
        subst <- substituteQuote[M].substitute(quote)
        // Check if we try to read from bundled channel
        unbndl <- subst.quote.get.singleBundle() match {
                   case Some(value) =>
                     if (!value.readFlag) {
                       interpreterErrorM[M].raiseError(
                         ReduceError("Trying to read from non-readable channel."))
                     } else {
                       interpreterErrorM[M].pure(Quote(value.body.get))
                     }
                   case None =>
                     interpreterErrorM[M].pure(subst)
                 }
      } yield unbndl

    def eval(bundle: Bundle)(implicit env: Env[Par]): M[Unit] =
      eval(bundle.body.get)

    /**
      * Continue is straightforward in this case, it just calls eval.
      */
    def continue(body: Par)(implicit env: Env[Par]): M[Unit] =
      eval(body)

    def evalExprToPar(expr: Expr)(implicit env: Env[Par]): M[Par] =
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
                          case None =>
                            interpreterErrorM[M].raiseError(
                              ReduceError("Unimplemented method: " + method))
                          case Some(f) => f(target.get, evaledArgs)(env)
                        }
          } yield resultPar
        }
        case EEvalBody(chan) => eval(chan).map(q => q.value)
        case _               => evalExprToExpr(expr).map(e => (fromExpr(e)(identity)))
      }

    def evalExprToExpr(expr: Expr)(implicit env: Env[Par]): M[Expr] = {
      def relop(p1: Par,
                p2: Par,
                relopb: (Boolean, Boolean) => Boolean,
                relopi: (Int, Int) => Boolean,
                relops: (String, String) => Boolean): M[Expr] =
        for {
          v1 <- evalSingleExpr(p1)
          v2 <- evalSingleExpr(p2)
          result <- (v1.exprInstance, v2.exprInstance) match {
                     case (GBool(b1), GBool(b2))     => Applicative[M].pure(GBool(relopb(b1, b2)))
                     case (GInt(i1), GInt(i2))       => Applicative[M].pure(GBool(relopi(i1, i2)))
                     case (GString(s1), GString(s2)) => Applicative[M].pure(GBool(relops(s1, s2)))
                     case _ =>
                       interpreterErrorM[M].raiseError(
                         ReduceError("Unexpected compare: " + v1 + " vs. " + v2))
                   }
        } yield result

      expr.exprInstance match {
        case x: GBool      => Applicative[M].pure[Expr](x)
        case x: GInt       => Applicative[M].pure[Expr](x)
        case x: GString    => Applicative[M].pure[Expr](x)
        case x: GUri       => Applicative[M].pure[Expr](x)
        case x: GByteArray => Applicative[M].pure[Expr](x)
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
            sv1 <- substitutePar[M].substitute(v1)
            sv2 <- substitutePar[M].substitute(v2)
          } yield GBool(sv1 == sv2)
        case ENeqBody(ENeq(p1, p2)) =>
          for {
            v1  <- evalExpr(p1.get)
            v2  <- evalExpr(p2.get)
            sv1 <- substitutePar[M].substitute(v1)
            sv2 <- substitutePar[M].substitute(v2)
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
        case ETupleBody(el) =>
          for {
            evaledPs  <- el.ps.toList.traverse(expr => evalExpr(expr))
            updatedPs = evaledPs.map(updateLocallyFree)
          } yield updateLocallyFree(ETuple(updatedPs, el.locallyFree, el.connectiveUsed))
        case EMethodBody(EMethod(method, target, arguments, _, _)) => {
          val methodLookup = methodTable(method)
          for {
            evaledTarget <- evalExpr(target.get)
            evaledArgs   <- arguments.toList.traverse(expr => evalExpr(expr))
            resultPar <- methodLookup match {
                          case None =>
                            interpreterErrorM[M].raiseError(
                              ReduceError("Unimplemented method: " + method))
                          case Some(f) => f(target.get, evaledArgs)(env)
                        }
            resultExpr <- evalSingleExpr(resultPar)
          } yield resultExpr
        }
        case EEvalBody(chan) =>
          for {
            q      <- eval(chan)
            result <- evalSingleExpr(q.value)
          } yield result
        case _ => interpreterErrorM[M].raiseError(ReduceError("Unimplemented expression: " + expr))
      }
    }

    private[this] def nth: (Par, Seq[Par]) => Env[Par] => M[Par] = {
      def localNth(ps: Seq[Par], nth: Int): Either[ReduceError, Par] =
        if (ps.isDefinedAt(nth)) {
          Right(ps(nth))
        } else {
          Left(ReduceError("Error: index out of bound: " + nth))
        }

      (p: Par, args: Seq[Par]) => (env: Env[Par]) =>
        {
          if (args.length != 1) {
            interpreterErrorM[M].raiseError(ReduceError("Error: nth expects 1 argument"))
          } else {
            for {
              nth <- evalToInt(args(0))(env)
              v   <- evalSingleExpr(p)(env)
              result <- v.exprInstance match {
                         case EListBody(EList(ps, _, _, _)) =>
                           interpreterErrorM[M].fromEither(localNth(ps, nth))
                         case ETupleBody(ETuple(ps, _, _)) =>
                           interpreterErrorM[M].fromEither(localNth(ps, nth))
                         case _ =>
                           interpreterErrorM[M].raiseError(
                             ReduceError(
                               "Error: nth applied to something that wasn't a list or tuple."))
                       }
            } yield result
          }
        }
    }

    private[this] def toByteArray: (Par, Seq[Par]) => Env[Par] => M[Par] = {
      def serialize(p: Par): Either[ReduceError, Array[Byte]] =
        Either
          .fromTry(Try(Serialize[Par].encode(p)))
          .leftMap(th =>
            ReduceError(s"Error: exception thrown when serializing $p." + th.getMessage))

      (p: Par, args: Seq[Par]) => (env: Env[Par]) =>
        {
          if (args.nonEmpty) {
            interpreterErrorM[M].raiseError(
              ReduceError("Error: toByteArray does not take arguments"))
          } else {
            evalExpr(p)(env)
              .map(serialize(_))
              .flatMap(interpreterErrorM[M].fromEither)
              .map(b => Expr(GByteArray(ByteString.copyFrom(b))))
          }
        }
    }

    private[this] def hexToBytes: (Par, Seq[Par]) => Env[Par] => M[Par] = {
      (p: Par, args: Seq[Par]) => (env: Env[Par]) =>
        {
          if (args.nonEmpty) {
            interpreterErrorM[M].raiseError(
              ReduceError("Error: hexToBytes does not take arguments"))
          } else {
            p.singleExpr() match {
              case Some(Expr(GString(encoded))) =>
                def decodingError(th: Throwable) =
                  ReduceError(
                    s"Error: exception was thrown when decoding input string to hexadecimal: ${th.getMessage}")
                Try(Expr(GByteArray(ByteString.copyFrom(Base16.decode(encoded)))))
                  .fold(th => interpreterErrorM[M].raiseError[Par](decodingError(th)),
                        x => interpreterErrorM[M].pure[Par](x))
              case _ =>
                interpreterErrorM[M].raiseError(
                  ReduceError("Error: hexToBytes can be called only on single strings."))
            }
          }
        }
    }

    def methodTable(method: String): Option[(Par, Seq[Par]) => Env[Par] => M[Par]] =
      method match {
        case "nth"         => Some(nth)
        case "toByteArray" => Some(toByteArray)
        case "hexToBytes"  => Some(hexToBytes)
        case _             => None
      }

    def evalSingleExpr(p: Par)(implicit env: Env[Par]): M[Expr] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
        interpreterErrorM[M].raiseError(
          ReduceError("Error: parallel or non expression found where expression expected."))
      else
        p.exprs match {
          case (e: Expr) +: Nil => evalExprToExpr(e)
          case _ =>
            interpreterErrorM[M].raiseError(ReduceError("Error: Multiple expressions given."))
        }

    def evalToInt(p: Par)(implicit env: Env[Par]): M[Int] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
        interpreterErrorM[M].raiseError(
          ReduceError("Error: parallel or non expression found where expression expected."))
      else
        p.exprs match {
          case Expr(GInt(v)) +: Nil => Applicative[M].pure(v)
          case Expr(EVarBody(EVar(v))) +: Nil =>
            for {
              p      <- eval(v.get)
              intVal <- evalToInt(p)
            } yield intVal
          case (e: Expr) +: Nil =>
            for {
              evaled <- evalExprToExpr(e)
              result <- evaled.exprInstance match {
                         case GInt(v) => Applicative[M].pure(v)
                         case _ =>
                           interpreterErrorM[M].raiseError(
                             ReduceError("Error: expression didn't evaluate to integer."))
                       }
            } yield result
          case _ =>
            interpreterErrorM[M].raiseError(
              ReduceError("Error: Integer expected, or unimplemented expression."))
        }

    def evalToBool(p: Par)(implicit env: Env[Par]): M[Boolean] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty)
        interpreterErrorM[M].raiseError(
          ReduceError("Error: parallel or non expression found where expression expected."))
      else
        p.exprs match {
          case Expr(GBool(b)) +: Nil => Applicative[M].pure(b)
          case Expr(EVarBody(EVar(v))) +: Nil =>
            for {
              p       <- eval(v.get)
              boolVal <- evalToBool(p)
            } yield boolVal
          case (e: Expr) +: Nil =>
            for {
              evaled <- evalExprToExpr(e)
              result <- evaled.exprInstance match {
                         case GBool(b) => Applicative[M].pure(b)
                         case _ =>
                           interpreterErrorM[M].raiseError(
                             ReduceError("Error: expression didn't evaluate to boolean."))
                       }
            } yield result
          case _ =>
            interpreterErrorM[M].raiseError(ReduceError("Error: Multiple expressions given."))
        }

    def updateLocallyFree(par: Par): Par = {
      val resultLocallyFree =
        par.sends.foldLeft(BitSet())((acc, send) => acc | send.locallyFree) |
          par.receives.foldLeft(BitSet())((acc, receive) => acc | receive.locallyFree) |
          par.news.foldLeft(BitSet())((acc, newProc) => acc | newProc.locallyFree) |
          par.exprs.foldLeft(BitSet())((acc, expr) => acc | ExprLocallyFree.locallyFree(expr)) |
          par.matches.foldLeft(BitSet())((acc, matchProc) => acc | matchProc.locallyFree)
      par.copy(locallyFree = resultLocallyFree)
    }

    def updateLocallyFree(elist: EList): EList = {
      val resultLocallyFree = elist.ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
      elist.copy(locallyFree = resultLocallyFree)
    }

    def updateLocallyFree(elist: ETuple): ETuple = {
      val resultLocallyFree = elist.ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
      elist.copy(locallyFree = resultLocallyFree)
    }

    /**
      * Evaluate any top level expressions in @param Par .
      */
    def evalExpr(par: Par)(implicit env: Env[Par]): M[Par] =
      for {
        evaledExprs <- par.exprs.toList.traverse(expr => evalExprToPar(expr))
        result = evaledExprs.foldLeft(par.copy(exprs = Vector())) { (acc, newPar) =>
          acc ++ newPar
        }
      } yield result
  }
}
