package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits._
import cats.mtl.FunctorTell
import cats.{Applicative, FlatMap, Foldable, Parallel, Eval => _}
import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.serialization.implicits._
import coop.rchain.models.{Match, MatchCase, _}
import coop.rchain.rholang.interpreter.Substitute._
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.matcher._
import coop.rchain.rholang.interpreter.storage.TuplespaceAlg
import coop.rchain.rspace.Serialize
import monix.eval.Coeval
import SpatialMatcher.spatialMatchAndCharge
import scala.collection.immutable.BitSet
import scala.util.Try

// Notes: Caution, a type annotation is often needed for Env.

/** Reduce is the interface for evaluating Rholang expressions.
  *
  * @tparam M The kind of Monad used for evaluation.
  */
trait Reduce[M[_]] {

  def eval(par: Par)(
      implicit env: Env[Par],
      rand: Blake2b512Random,
      costAccountingAlg: CostAccountingAlg[M]
  ): M[Unit]

  def inj(
      par: Par
  )(implicit rand: Blake2b512Random, costAccountingAlg: CostAccountingAlg[M]): M[Unit]

  /**
    * Evaluate any top level expressions in @param Par .
    */
  def evalExpr(par: Par)(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par]

  def evalExprToPar(
      expr: Expr
  )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par]
}

// TODO:
// In a perfect world we would an algebra of an interpreter encoded in the terms of
// rholang's elimination and have the `ChargingReducer` forward the calls to the plain algebra
// and charge accordingly. This would eliminate the "charge" calls from the algebra of a language.
class ChargingReducer[M[_]](implicit R: Reduce[M], C: CostAccountingAlg[M]) {
  def getAvailablePhlos(): M[CostAccount] =
    C.get()

  def setAvailablePhlos(limit: Cost): M[Unit] =
    C.set(CostAccount(0, limit))

  def eval(par: Par)(implicit env: Env[Par], rand: Blake2b512Random): M[Unit] =
    R.eval(par)

  def inj(par: Par)(implicit rand: Blake2b512Random): M[Unit] =
    R.inj(par)

  def evalExpr(par: Par)(implicit env: Env[Par]): M[Par] =
    R.evalExpr(par)

  def evalExprToPar(expr: Expr)(implicit env: Env[Par]): M[Par] =
    R.evalExprToPar(expr)
}

object Reduce {

  class DebruijnInterpreter[M[_], F[_]](
      tuplespaceAlg: TuplespaceAlg[M],
      private val urnMap: Map[String, Par]
  )(
      implicit
      parallel: cats.Parallel[M, F],
      s: Sync[M],
      fTell: FunctorTell[M, Throwable]
  ) extends Reduce[M] {

    /**
      * Materialize a send in the store, optionally returning the matched continuation.
      *
      * @param chan  The channel on which data is being sent.
      * @param data  The par objects holding the processes being sent.
      * @param persistent  True if the write should remain in the tuplespace indefinitely.
      * @param env  An environment marking the execution context.
      * @return  An optional continuation resulting from a match in the tuplespace.
      */
    private def produce(
        chan: Par,
        data: Seq[Par],
        persistent: Boolean,
        rand: Blake2b512Random
    )(implicit costAccountingAlg: CostAccountingAlg[M]): M[Unit] =
      tuplespaceAlg.produce(chan, ListParWithRandom(data, rand), persistent)

    /**
      * Materialize a send in the store, optionally returning the matched continuation.
      *
      * @param binds  A Seq of pattern, channel pairs. Each pattern is a Seq[Par].
      *               The Seq is for arity matching, and each term in the Seq is a name pattern.
      * @param body  A Par object which will be run in the envirnoment resulting from the match.
      * @param env  The current environment, to which the matches will be added before resuming
      *             execution in body
      * @return  An optional continuation resulting from a match. The body of the continuation
      *          will be @param body if the continuation is not None.
      */
    private def consume(
        binds: Seq[(BindPattern, Par)],
        body: Par,
        persistent: Boolean,
        rand: Blake2b512Random
    )(implicit costAccountingAlg: CostAccountingAlg[M]): M[Unit] =
      tuplespaceAlg.consume(binds, ParWithRandom(body, rand), persistent)

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
    override def eval(par: Par)(
        implicit env: Env[Par],
        rand: Blake2b512Random,
        costAccountingAlg: CostAccountingAlg[M]
    ): M[Unit] = {
      val filteredExprs = par.exprs.filter { expr =>
        expr.exprInstance match {
          case _: EVarBody    => true
          case _: EMethodBody => true
          case _              => false
        }
      }
      val starts = Vector(
        par.sends.size,
        par.receives.size,
        par.news.size,
        par.matches.size,
        par.bundles.size,
        filteredExprs.size
      ).scanLeft(0)(_ + _)
      def handle[A](
          eval: (A => (Env[Par], Blake2b512Random, CostAccountingAlg[M]) => M[Unit]),
          start: Int
      )(ta: (A, Int)): M[Unit] = {
        val newRand =
          if (starts(6) == 1)
            rand
          else if (starts(6) > 256)
            rand.splitShort((start + ta._2).toShort)
          else
            rand.splitByte((start + ta._2).toByte)
        eval(ta._1)(env, newRand, costAccountingAlg).handleErrorWith {
          case e: OutOfPhlogistonsError.type =>
            s.raiseError(e)
          case e =>
            fTell.tell(e) *> s.unit
        }
      }
      List(
        Parallel.parTraverse(par.sends.zipWithIndex.toList)(handle(evalExplicit, starts(0))),
        Parallel.parTraverse(par.receives.zipWithIndex.toList)(handle(evalExplicit, starts(1))),
        Parallel.parTraverse(par.news.zipWithIndex.toList)(handle(evalExplicit, starts(2))),
        Parallel.parTraverse(par.matches.zipWithIndex.toList)(handle(evalExplicit, starts(3))),
        Parallel.parTraverse(par.bundles.zipWithIndex.toList)(handle(evalExplicit, starts(4))),
        Parallel.parTraverse(filteredExprs.zipWithIndex.toList)(texpr => {
          val (expr, idx) = texpr
          val newRand =
            if (starts(6) == 1)
              rand
            else if (starts(6) > 256)
              rand.splitShort((starts(5) + idx).toShort)
            else
              rand.splitByte((starts(5) + idx).toByte)
          expr.exprInstance match {
            case EVarBody(EVar(v)) =>
              (for {
                varref <- eval(v)
                _      <- eval(varref)(env, newRand, costAccountingAlg)
              } yield ()).handleError(fTell.tell)
            case e: EMethodBody =>
              (for {
                p <- evalExprToPar(Expr(e))
                _ <- eval(p)(env, newRand, costAccountingAlg)
              } yield ()).handleError(fTell.tell)
            case _ => s.unit
          }
        })
      ).parSequence.as(Unit)
    }

    override def inj(
        par: Par
    )(implicit rand: Blake2b512Random, costAccountingAlg: CostAccountingAlg[M]): M[Unit] =
      for {
        _ <- eval(par)(Env[Par](), rand, costAccountingAlg)
      } yield ()

    private def evalExplicit(
        send: Send
    )(env: Env[Par], rand: Blake2b512Random, costAccountingAlg: CostAccountingAlg[M]): M[Unit] =
      eval(send)(env, rand, costAccountingAlg)

    /** Algorithm as follows:
      *
      * 1. Fully evaluate the channel in given environment.
      * 2. Substitute any variable references in the channel so that it can be
      *    correctly used as a key in the tuple space.
      * 3. Evaluate any top level expressions in the data being sent.
      * 4. Call produce
      * 5. If produce returned a continuation, evaluate it.
      * @param send An output process
      * @param env An execution context
      * @return
      */
    private def eval(send: Send)(
        implicit env: Env[Par],
        rand: Blake2b512Random,
        costAccountingAlg: CostAccountingAlg[M]
    ): M[Unit] =
      for {
        evalChan <- evalExpr(send.chan)
        subChan  <- substituteAndCharge[Par, M](evalChan, 0, env)
        unbundled <- subChan.singleBundle() match {
                      case Some(value) =>
                        if (!value.writeFlag) {
                          s.raiseError(ReduceError("Trying to send on non-writeable channel."))
                        } else {
                          s.pure(value.body)
                        }
                      case None => Applicative[M].pure(subChan)
                    }

        data <- send.data.toList.traverse(x => evalExpr(x))
        substData <- data.traverse(
                      p => substituteAndCharge[Par, M](p, 0, env)
                    )
        _ <- produce(unbundled, substData, send.persistent, rand)
        _ <- costAccountingAlg.charge(SEND_EVAL_COST)
      } yield ()

    private def evalExplicit(
        receive: Receive
    )(env: Env[Par], rand: Blake2b512Random, costAccountingAlg: CostAccountingAlg[M]): M[Unit] =
      eval(receive)(env, rand, costAccountingAlg)

    private def eval(receive: Receive)(
        implicit env: Env[Par],
        rand: Blake2b512Random,
        costAccountingAlg: CostAccountingAlg[M]
    ): M[Unit] =
      for {
        binds <- receive.binds.toList
                  .traverse(
                    rb =>
                      for {
                        q <- unbundleReceive(rb)
                        substPatterns <- rb.patterns.toList.traverse(
                                          pattern =>
                                            substituteAndCharge[Par, M](
                                              pattern,
                                              1,
                                              env
                                            )
                                        )
                      } yield (BindPattern(substPatterns, rb.remainder, rb.freeCount), q)
                  )
        // TODO: Allow for the environment to be stored with the body in the Tuplespace
        substBody <- substituteNoSortAndCharge[Par, M](
                      receive.body,
                      0,
                      env.shift(receive.bindCount),
                      costAccountingAlg
                    )
        _ <- consume(binds, substBody, receive.persistent, rand)
        _ <- costAccountingAlg.charge(RECEIVE_EVAL_COST)
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
    private def eval(
        valproc: Var
    )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
      costAccountingAlg.charge(VAR_EVAL_COST) *> {
        valproc.varInstance match {
          case BoundVar(level) =>
            env.get(level) match {
              case Some(par) =>
                s.pure(par)
              case None =>
                s.raiseError(ReduceError("Unbound variable: " + level + " in " + env.envMap))
            }
          case Wildcard(_) =>
            s.raiseError(ReduceError("Unbound variable: attempting to evaluate a pattern"))
          case FreeVar(_) =>
            s.raiseError(ReduceError("Unbound variable: attempting to evaluate a pattern"))
          case VarInstance.Empty =>
            s.raiseError(ReduceError("Impossible var instance EMPTY"))
        }
      }

    private def evalExplicit(
        mat: Match
    )(env: Env[Par], rand: Blake2b512Random, costAccountingAlg: CostAccountingAlg[M]): M[Unit] =
      eval(mat)(env, rand, costAccountingAlg)
    private def eval(mat: Match)(
        implicit env: Env[Par],
        rand: Blake2b512Random,
        costAccountingAlg: CostAccountingAlg[M]
    ): M[Unit] = {
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

      def firstMatch(target: Par, cases: Seq[MatchCase])(implicit env: Env[Par]): M[Unit] = {
        def firstMatchM(
            state: Tuple2[Par, Seq[MatchCase]]
        ): M[Either[Tuple2[Par, Seq[MatchCase]], Unit]] = {
          val (target, cases) = state
          cases match {
            case Nil => Applicative[M].pure(Right(()))
            case singleCase +: caseRem =>
              for {
                pattern <- substituteAndCharge[Par, M](
                            singleCase.pattern,
                            1,
                            env
                          )
                matchResult <- spatialMatchAndCharge[M](target, pattern)
                res <- matchResult match {
                        case None =>
                          Applicative[M].pure(Left((target, caseRem)))
                        case Some((freeMap, _)) =>
                          val newEnv: Env[Par] = addToEnv(env, freeMap, singleCase.freeCount)
                          eval(singleCase.source)(newEnv, implicitly, costAccountingAlg)
                            .map(Right(_))
                      }
              } yield res
          }
        }
        FlatMap[M].tailRecM[Tuple2[Par, Seq[MatchCase]], Unit]((target, cases))(firstMatchM)
      }

      for {
        evaledTarget <- evalExpr(mat.target)
        // TODO(kyle): Make the matcher accept an environment, instead of substituting it.
        substTarget <- substituteAndCharge[Par, M](evaledTarget, 0, env)
        _           <- firstMatch(substTarget, mat.cases)
        _           <- costAccountingAlg.charge(MATCH_EVAL_COST)
      } yield ()
    }

    /**
      * Adds neu.bindCount new GPrivate from UUID's to the environment and then
      * proceeds to evaluate the body.
      *
      * @param neu
      * @return
      */
    private def evalExplicit(
        neu: New
    )(env: Env[Par], rand: Blake2b512Random, costAccountingAlg: CostAccountingAlg[M]): M[Unit] =
      eval(neu)(env, rand, costAccountingAlg)
    private def eval(neu: New)(
        implicit env: Env[Par],
        rand: Blake2b512Random,
        costAccountingAlg: CostAccountingAlg[M]
    ): M[Unit] = {
      def alloc(count: Int, urns: Seq[String]): M[Env[Par]] = {
        val simpleNews = (0 until (count - urns.size)).toList.foldLeft(env) { (_env, _) =>
          val addr: Par = GPrivate(ByteString.copyFrom(rand.next()))
          _env.put(addr)
        }
        def addUrn(newEnv: Env[Par], urn: String): Either[ReduceError, Env[Par]] =
          urnMap.get(urn) match {
            case Some(p) => Right(newEnv.put(p))
            case None    => Left(ReduceError(s"Unknown urn for new: ${urn}"))
          }
        def foldLeftEarly[A, B, E](
            init: B,
            as: Seq[A],
            cata: (B, A) => Either[E, B]
        ): Either[E, B] =
          Foldable[List].foldM(as.toList, init)(cata)
        foldLeftEarly(simpleNews, urns, addUrn) match {
          case Right(env) => Applicative[M].pure(env)
          case Left(e)    => s.raiseError(e)
        }
      }

      costAccountingAlg.charge(newBindingsCost(neu.bindCount)) *>
        alloc(neu.bindCount, neu.uri).flatMap { newEnv =>
          eval(neu.p)(newEnv, rand, costAccountingAlg)
        }
    }

    private[this] def unbundleReceive(
        rb: ReceiveBind
    )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
      for {
        evalSrc <- evalExpr(rb.source)
        subst   <- substituteAndCharge[Par, M](evalSrc, 0, env)
        // Check if we try to read from bundled channel
        unbndl <- subst.singleBundle() match {
                   case Some(value) =>
                     if (!value.readFlag) {
                       s.raiseError(ReduceError("Trying to read from non-readable channel."))
                     } else {
                       s.pure(value.body)
                     }
                   case None =>
                     s.pure(subst)
                 }
      } yield unbndl

    private def evalExplicit(
        bundle: Bundle
    )(env: Env[Par], rand: Blake2b512Random, costAccountingAlg: CostAccountingAlg[M]): M[Unit] =
      eval(bundle)(env, rand, costAccountingAlg)
    private def eval(bundle: Bundle)(
        implicit env: Env[Par],
        rand: Blake2b512Random,
        costAccountingAlg: CostAccountingAlg[M]
    ): M[Unit] =
      eval(bundle.body)

    def evalExprToPar(
        expr: Expr
    )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
      expr.exprInstance match {
        case EVarBody(EVar(v)) =>
          for {
            p       <- eval(v)
            evaledP <- evalExpr(p)
          } yield evaledP
        case EMethodBody(EMethod(method, target, arguments, _, _)) => {
          val methodLookup = methodTable.get(method)
          for {
            _            <- costAccountingAlg.charge(METHOD_CALL_COST)
            evaledTarget <- evalExpr(target)
            evaledArgs   <- arguments.toList.traverse(expr => evalExpr(expr))
            resultPar <- methodLookup match {
                          case None =>
                            s.raiseError(ReduceError("Unimplemented method: " + method))
                          case Some(f) => f(evaledTarget, evaledArgs)
                        }
          } yield resultPar
        }
        case _ => evalExprToExpr(expr).map(e => fromExpr(e)(identity))
      }

    private def evalExprToExpr(
        expr: Expr
    )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Expr] = Sync[M].defer {
      def relop(
          p1: Par,
          p2: Par,
          relopb: (Boolean, Boolean) => Boolean,
          relopi: (Long, Long) => Boolean,
          relops: (String, String) => Boolean
      ): M[Expr] =
        for {
          v1 <- evalSingleExpr(p1)
          v2 <- evalSingleExpr(p2)
          result <- (v1.exprInstance, v2.exprInstance) match {
                     case (GBool(b1), GBool(b2)) =>
                       Applicative[M].pure(GBool(relopb(b1, b2)))
                     case (GInt(i1), GInt(i2)) =>
                       Applicative[M].pure(GBool(relopi(i1, i2)))
                     case (GString(s1), GString(s2)) =>
                       Applicative[M].pure(GBool(relops(s1, s2)))
                     case _ =>
                       s.raiseError(ReduceError("Unexpected compare: " + v1 + " vs. " + v2))
                   }
        } yield result

      expr.exprInstance match {
        case x: GBool =>
          Applicative[M].pure[Expr](x)
        case x: GInt =>
          Applicative[M].pure[Expr](x)
        case x: GString =>
          Applicative[M].pure[Expr](x)
        case x: GUri =>
          Applicative[M].pure[Expr](x)
        case x: GByteArray =>
          Applicative[M].pure[Expr](x)
        case ENotBody(ENot(p)) =>
          for {
            b <- evalToBool(p)
          } yield GBool(!b)
        case ENegBody(ENeg(p)) =>
          for {
            v <- evalToLong(p)
          } yield GInt(-v)
        case EMultBody(EMult(p1, p2)) =>
          for {
            v1 <- evalToLong(p1)
            v2 <- evalToLong(p2)
            _  <- costAccountingAlg.charge(MULTIPLICATION_COST)
          } yield GInt(v1 * v2)
        case EDivBody(EDiv(p1, p2)) =>
          for {
            v1 <- evalToLong(p1)
            v2 <- evalToLong(p2)
            _  <- costAccountingAlg.charge(DIVISION_COST)
          } yield GInt(v1 / v2)
        case EPlusBody(EPlus(p1, p2)) =>
          for {
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (GInt(lhs), GInt(rhs)) =>
                         for {
                           _ <- costAccountingAlg.charge(SUM_COST)
                         } yield Expr(GInt(lhs + rhs))
                       case (lhs: ESetBody, rhs) =>
                         for {
                           _         <- costAccountingAlg.charge(OP_CALL_COST)
                           resultPar <- add(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (_: GInt, other) =>
                         s.raiseError(OperatorExpectedError("+", "Int", other.typ))
                       case (other, _) =>
                         s.raiseError(OperatorNotDefined("+", other.typ))
                     }
          } yield result
        case EMinusBody(EMinus(p1, p2)) =>
          for {
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (GInt(lhs), GInt(rhs)) =>
                         for {
                           _ <- costAccountingAlg.charge(SUBTRACTION_COST)
                         } yield Expr(GInt(lhs - rhs))
                       case (lhs: EMapBody, rhs) =>
                         for {
                           _         <- costAccountingAlg.charge(OP_CALL_COST)
                           resultPar <- delete(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (lhs: ESetBody, rhs) =>
                         for {
                           _         <- costAccountingAlg.charge(OP_CALL_COST)
                           resultPar <- delete(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (_: GInt, other) =>
                         s.raiseError(OperatorExpectedError("-", "Int", other.typ))
                       case (other, _) =>
                         s.raiseError(OperatorNotDefined("-", other.typ))
                     }
          } yield result
        case ELtBody(ELt(p1, p2)) =>
          relop(p1, p2, (_ < _), (_ < _), (_ < _)) <* costAccountingAlg.charge(COMPARISON_COST)
        case ELteBody(ELte(p1, p2)) =>
          relop(p1, p2, (_ <= _), (_ <= _), (_ <= _)) <* costAccountingAlg.charge(COMPARISON_COST)
        case EGtBody(EGt(p1, p2)) =>
          relop(p1, p2, (_ > _), (_ > _), (_ > _)) <* costAccountingAlg.charge(COMPARISON_COST)
        case EGteBody(EGte(p1, p2)) =>
          relop(p1, p2, (_ >= _), (_ >= _), (_ >= _)) <* costAccountingAlg.charge(COMPARISON_COST)
        case EEqBody(EEq(p1, p2)) =>
          for {
            v1 <- evalExpr(p1)
            v2 <- evalExpr(p2)
            // TODO: build an equality operator that takes in an environment.
            sv1 <- substituteAndCharge[Par, M](v1, 0, env)
            sv2 <- substituteAndCharge[Par, M](v2, 0, env)
            _   <- costAccountingAlg.charge(equalityCheckCost(sv1, sv2))
          } yield GBool(sv1 == sv2)
        case ENeqBody(ENeq(p1, p2)) =>
          for {
            v1  <- evalExpr(p1)
            v2  <- evalExpr(p2)
            sv1 <- substituteAndCharge[Par, M](v1, 0, env)
            sv2 <- substituteAndCharge[Par, M](v2, 0, env)
            _   <- costAccountingAlg.charge(equalityCheckCost(sv1, sv2))
          } yield GBool(sv1 != sv2)
        case EAndBody(EAnd(p1, p2)) =>
          for {
            b1 <- evalToBool(p1)
            b2 <- evalToBool(p2)
            _  <- costAccountingAlg.charge(BOOLEAN_AND_COST)
          } yield GBool(b1 && b2)
        case EOrBody(EOr(p1, p2)) =>
          for {
            b1 <- evalToBool(p1)
            b2 <- evalToBool(p2)
            _  <- costAccountingAlg.charge(BOOLEAN_OR_COST)
          } yield GBool(b1 || b2)

        case EMatchesBody(EMatches(target, pattern)) =>
          for {
            evaledTarget <- evalExpr(target)
            substTarget  <- substituteAndCharge[Par, M](evaledTarget, 0, env)
            substPattern <- substituteAndCharge[Par, M](pattern, 1, env)
            matchResult  <- spatialMatchAndCharge[M](substTarget, substPattern)
          } yield GBool(matchResult.isDefined)

        case EPercentPercentBody(EPercentPercent(p1, p2)) =>
          def evalToStringPair(keyExpr: Expr, valueExpr: Expr): M[(String, String)] =
            (keyExpr.exprInstance, valueExpr.exprInstance) match {
              case (GString(keyString), GString(valueString)) =>
                Applicative[M].pure[(String, String)](keyString -> valueString)
              case (GString(keyString), GInt(valueInt)) =>
                Applicative[M].pure[(String, String)](keyString -> valueInt.toString)
              // TODO: Add cases for other ground terms as well? Maybe it would be better
              // to implement cats.Show for all ground terms.
              case (_: GString, value) =>
                s.raiseError[(String, String)](
                  ReduceError(s"Error: interpolation doesn't support ${value.typ}")
                )
              case _ =>
                s.raiseError[(String, String)](
                  ReduceError("Error: interpolation Map should only contain String keys")
                )
            }
          def interpolate(string: String, keyValuePairs: List[(String, String)]): String = {
            val result  = StringBuilder.newBuilder
            var current = string
            while (current.nonEmpty) {
              keyValuePairs.find {
                case (k, _) => current.startsWith("${" + k + "}")
              } match {
                case Some((k, v)) =>
                  result ++= v
                  current = current.drop(k.length + 3)
                case None =>
                  result += current.head
                  current = current.tail
              }
            }
            result.toString
          }
          for {
            _  <- costAccountingAlg.charge(OP_CALL_COST)
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (GString(lhs), EMapBody(ParMap(rhs, _, _, _))) =>
                         for {
                           result <- rhs.iterator
                                      .map {
                                        case (k, v) =>
                                          for {
                                            keyExpr   <- evalSingleExpr(k)
                                            valueExpr <- evalSingleExpr(v)
                                            result    <- evalToStringPair(keyExpr, valueExpr)
                                          } yield result
                                      }
                                      .toList
                                      .sequence[M, (String, String)]
                                      .map(
                                        keyValuePairs => GString(interpolate(lhs, keyValuePairs))
                                      )
                           _ <- costAccountingAlg.charge(LOOKUP_COST * lhs.length)
                         } yield result
                       case (_: GString, other) =>
                         s.raiseError(OperatorExpectedError("%%", "Map", other.typ))
                       case (other, _) =>
                         s.raiseError(OperatorNotDefined("%%", other.typ))
                     }
          } yield result
        case EPlusPlusBody(EPlusPlus(p1, p2)) =>
          for {
            _  <- costAccountingAlg.charge(OP_CALL_COST)
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (GString(lhs), GString(rhs)) =>
                         for {
                           _ <- costAccountingAlg.charge(
                                 STRING_APPEND_COST * (lhs.length + rhs.length)
                               )
                         } yield Expr(GString(lhs + rhs))
                       case (GByteArray(lhs), GByteArray(rhs)) =>
                         for {
                           _ <- costAccountingAlg.charge(
                                 PREPEND_COST * lhs.size()
                               )
                         } yield Expr(GByteArray(lhs.concat(rhs)))
                       case (EListBody(lhs), EListBody(rhs)) =>
                         for {
                           _ <- costAccountingAlg.charge(PREPEND_COST * lhs.ps.length)
                         } yield
                           Expr(
                             EListBody(
                               EList(
                                 lhs.ps ++ rhs.ps,
                                 lhs.locallyFree union rhs.locallyFree,
                                 lhs.connectiveUsed || rhs.connectiveUsed
                               )
                             )
                           )
                       case (lhs: EMapBody, rhs: EMapBody) =>
                         for {
                           resultPar <- union(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (lhs: ESetBody, rhs: ESetBody) =>
                         for {
                           resultPar <- union(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (_: GString, other) =>
                         s.raiseError(OperatorExpectedError("++", "String", other.typ))
                       case (_: EListBody, other) =>
                         s.raiseError(OperatorExpectedError("++", "List", other.typ))
                       case (_: EMapBody, other) =>
                         s.raiseError(OperatorExpectedError("++", "Map", other.typ))
                       case (_: ESetBody, other) =>
                         s.raiseError(OperatorExpectedError("++", "Set", other.typ))
                       case (other, _) =>
                         s.raiseError(OperatorNotDefined("++", other.typ))
                     }
          } yield result
        case EMinusMinusBody(EMinusMinus(p1, p2)) =>
          for {
            _  <- costAccountingAlg.charge(OP_CALL_COST)
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (lhs: ESetBody, rhs: ESetBody) =>
                         for {
                           resultPar <- diff(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (_: ESetBody, other) =>
                         s.raiseError(OperatorExpectedError("--", "Set", other.typ))
                       case (other, _) =>
                         s.raiseError(OperatorNotDefined("--", other.typ))
                     }
          } yield result
        case EVarBody(EVar(v)) =>
          for {
            p       <- eval(v)
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

        case ESetBody(set) =>
          for {
            evaledPs  <- set.ps.sortedPars.traverse(expr => evalExpr(expr))
            updatedPs = evaledPs.map(updateLocallyFree)
          } yield set.copy(ps = SortedParHashSet(updatedPs))

        case EMapBody(map) =>
          for {
            evaledPs <- map.ps.sortedMap.traverse {
                         case (key, value) =>
                           for {
                             eKey   <- evalExpr(key).map(updateLocallyFree)
                             eValue <- evalExpr(value).map(updateLocallyFree)
                           } yield (eKey, eValue)
                       }
          } yield map.copy(ps = SortedParMap(evaledPs))

        case EMethodBody(EMethod(method, target, arguments, _, _)) => {
          val methodLookup = methodTable.get(method)
          for {
            _            <- costAccountingAlg.charge(METHOD_CALL_COST)
            evaledTarget <- evalExpr(target)
            evaledArgs   <- arguments.toList.traverse(expr => evalExpr(expr))
            resultPar <- methodLookup match {
                          case None =>
                            s.raiseError(ReduceError("Unimplemented method: " + method))
                          case Some(f) => f(evaledTarget, evaledArgs)
                        }
            resultExpr <- evalSingleExpr(resultPar)
          } yield resultExpr
        }
        case _ => s.raiseError(ReduceError("Unimplemented expression: " + expr))
      }
    }

    private abstract class Method() {
      def apply(p: Par, args: Seq[Par])(
          implicit env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]
      ): M[Par]
    }

    private[this] val nth: Method = new Method() {
      def localNth(ps: Seq[Par], nth: Int): Either[ReduceError, Par] =
        if (ps.isDefinedAt(nth)) {
          Right(ps(nth))
        } else {
          Left(ReduceError("Error: index out of bound: " + nth))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        if (args.length != 1) {
          s.raiseError(ReduceError("Error: nth expects 1 argument"))
        } else {
          for {
            nthRaw <- evalToLong(args(0))
            nth    <- restrictToInt(nthRaw)
            v      <- evalSingleExpr(p)
            _      <- costAccountingAlg.charge(nthMethodCost(nth))
            result <- v.exprInstance match {
                       case EListBody(EList(ps, _, _, _)) =>
                         s.fromEither(localNth(ps, nth))
                       case ETupleBody(ETuple(ps, _, _)) =>
                         s.fromEither(localNth(ps, nth))
                       case _ =>
                         s.raiseError(
                           ReduceError(
                             "Error: nth applied to something that wasn't a list or tuple."
                           )
                         )
                     }
          } yield result
        }
    }

    private[this] val toByteArray: Method = new Method() {
      def serialize(p: Par): Either[ReduceError, Array[Byte]] =
        Either
          .fromTry(Try(Serialize[Par].encode(p).toArray))
          .leftMap(
            th => ReduceError(s"Error: exception thrown when serializing $p." + th.getMessage)
          )

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        if (args.nonEmpty) {
          s.raiseError(ReduceError("Error: toByteArray does not take arguments"))
        } else {
          for {
            exprEvaled <- evalExpr(p)
            exprSubst  <- substituteAndCharge[Par, M](exprEvaled, 0, env)
            _          <- costAccountingAlg.charge(toByteArrayCost(exprSubst))
            ba         <- s.fromEither(serialize(exprSubst))
          } yield Expr(GByteArray(ByteString.copyFrom(ba)))
        }
    }

    private[this] val hexToBytes: Method = new Method() {

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        if (args.nonEmpty) {
          s.raiseError(ReduceError("Error: hexToBytes does not take arguments"))
        } else {
          p.singleExpr() match {
            case Some(Expr(GString(encoded))) =>
              def decodingError(th: Throwable) =
                ReduceError(
                  s"Error: exception was thrown when decoding input string to hexadecimal: ${th.getMessage}"
                )
              for {
                _           <- costAccountingAlg.charge(hexToByteCost(encoded))
                encodingRes = Try(ByteString.copyFrom(Base16.decode(encoded)))
                res <- encodingRes.fold(
                        th => s.raiseError(decodingError(th)),
                        ba => Applicative[M].pure[Par](Expr(GByteArray(ba)))
                      )
              } yield res
            case _ =>
              s.raiseError(ReduceError("Error: hexToBytes can be called only on single strings."))
          }
        }
    }

    private[this] val toUtf8Bytes: Method = new Method() {

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        if (args.nonEmpty) {
          s.raiseError(ReduceError("Error: toUtf8Bytes does not take arguments"))
        } else {
          p.singleExpr() match {
            case Some(Expr(GString(encoded))) =>
              for {
                _ <- costAccountingAlg.charge(hexToByteCost(encoded))
              } yield Expr(GByteArray(ByteString.copyFrom(encoded.getBytes("UTF-8"))))
            case _ =>
              s.raiseError(ReduceError("Error: toUtf8Bytes can be called only on single strings."))
          }
        }
    }

    private[this] def method(methodName: String, expectedArgsLength: Int, args: Seq[Par])(
        thunk: => M[Par]
    ): M[Par] =
      if (args.length != expectedArgsLength) {
        s.raiseError(MethodArgumentNumberMismatch(methodName, expectedArgsLength, args.length))
      } else {
        thunk
      }

    private[this] val union: Method = new Method() {
      def locallyFreeUnion(base: Coeval[BitSet], other: Coeval[BitSet]): Coeval[BitSet] =
        base.flatMap(b => other.map(o => b | o))

      def union(baseExpr: Expr, otherExpr: Expr)(implicit costAccountingAlg: CostAccountingAlg[M]) =
        (baseExpr.exprInstance, otherExpr.exprInstance) match {
          case (
              ESetBody(base @ ParSet(basePs, _, _, _)),
              ESetBody(other @ ParSet(otherPs, _, _, _))
              ) =>
            costAccountingAlg.charge(ADD_COST * basePs.size) *> Applicative[M].pure[Expr](
              ESetBody(
                ParSet(
                  basePs.union(otherPs.sortedPars.toSet),
                  base.connectiveUsed || other.connectiveUsed,
                  locallyFreeUnion(base.locallyFree, other.locallyFree),
                  None
                )
              )
            )
          case (
              EMapBody(base @ ParMap(baseMap, _, _, _)),
              EMapBody(other @ ParMap(otherMap, _, _, _))
              ) =>
            costAccountingAlg.charge(ADD_COST * baseMap.size) *>
              Applicative[M].pure[Expr](
                EMapBody(
                  ParMap(
                    (baseMap ++ otherMap.sortedMap).toSeq,
                    base.connectiveUsed || other.connectiveUsed,
                    locallyFreeUnion(base.locallyFree, other.locallyFree),
                    None
                  )
                )
              )
          case (other, _) =>
            s.raiseError(MethodNotDefined("union", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 1)
                s.raiseError(MethodArgumentNumberMismatch("union", 1, args.length))
              else Applicative[M].unit
          baseExpr  <- evalSingleExpr(p)
          otherExpr <- evalSingleExpr(args(0))
          result    <- union(baseExpr, otherExpr)
        } yield result
    }

    private[this] val diff: Method = new Method() {
      def locallyFreeUnion(base: Coeval[BitSet], other: Coeval[BitSet]): Coeval[BitSet] =
        base.flatMap(b => other.map(o => b | o))

      def diff(baseExpr: Expr, otherExpr: Expr)(implicit costAccountingAlg: CostAccountingAlg[M]) =
        (baseExpr.exprInstance, otherExpr.exprInstance) match {
          case (
              ESetBody(base @ ParSet(basePs, _, _, _)),
              ESetBody(other @ ParSet(otherPs, _, _, _))
              ) =>
            // diff is implemented in terms of foldLeft that at each step
            // removes one element from the collection.
            costAccountingAlg.charge(REMOVE_COST * basePs.size) *>
              Applicative[M].pure[Expr](
                ESetBody(
                  ParSet(
                    basePs.diff(otherPs.sortedPars.toSet),
                    base.connectiveUsed || other.connectiveUsed,
                    locallyFreeUnion(base.locallyFree, other.locallyFree),
                    None
                  )
                )
              )
          case (EMapBody(ParMap(basePs, _, _, _)), EMapBody(ParMap(otherPs, _, _, _))) =>
            val newMap = basePs -- otherPs.keys
            costAccountingAlg.charge(REMOVE_COST * basePs.size) *>
              Applicative[M].pure[Expr](
                EMapBody(ParMap(newMap))
              )
          case (other, _) =>
            s.raiseError(MethodNotDefined("diff", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 1)
                s.raiseError(MethodArgumentNumberMismatch("diff", 1, args.length))
              else Applicative[M].unit
          baseExpr  <- evalSingleExpr(p)
          otherExpr <- evalSingleExpr(args(0))
          result    <- diff(baseExpr, otherExpr)
        } yield result
    }

    private[this] val add: Method = new Method() {
      def add(baseExpr: Expr, par: Par)(implicit costAccountingAlg: CostAccountingAlg[M]) =
        baseExpr.exprInstance match {
          case ESetBody(base @ ParSet(basePs, _, _, _)) =>
            Applicative[M].pure[Expr](
              ESetBody(
                ParSet(
                  basePs + par,
                  base.connectiveUsed || par.connectiveUsed,
                  base.locallyFree.map(b => b | par.locallyFree),
                  None
                )
              )
            )
          //TODO(mateusz.gorski): think whether cost accounting for addition should be dependend on the operands

          case other =>
            s.raiseError(MethodNotDefined("add", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 1)
                s.raiseError(MethodArgumentNumberMismatch("add", 1, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          element  <- evalExpr(args(0))
          result   <- add(baseExpr, element)
          _        <- costAccountingAlg.charge(ADD_COST)
        } yield result
    }

    private[this] val delete: Method = new Method() {
      def delete(baseExpr: Expr, par: Par): M[Expr] =
        baseExpr.exprInstance match {
          case ESetBody(base @ ParSet(basePs, _, _, _)) =>
            Applicative[M].pure[Expr](
              ESetBody(
                ParSet(
                  basePs - par,
                  base.connectiveUsed || par.connectiveUsed,
                  base.locallyFree.map(b => b | par.locallyFree),
                  None
                )
              )
            )
          case EMapBody(base @ ParMap(basePs, _, _, _)) =>
            Applicative[M].pure[Expr](
              EMapBody(
                ParMap(
                  basePs - par,
                  base.connectiveUsed || par.connectiveUsed,
                  base.locallyFree.map(b => b | par.locallyFree),
                  None
                )
              )
            )
          case other =>
            s.raiseError(MethodNotDefined("delete", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 1)
                s.raiseError(MethodArgumentNumberMismatch("delete", 1, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          element  <- evalExpr(args(0))
          result   <- delete(baseExpr, element)
          _        <- costAccountingAlg.charge(REMOVE_COST) //TODO(mateusz.gorski): think whether deletion of an element from the collection should dependent on the collection type/size
        } yield result
    }

    private[this] val contains: Method = new Method() {
      def contains(baseExpr: Expr, par: Par): M[Expr] =
        baseExpr.exprInstance match {
          case ESetBody(ParSet(basePs, _, _, _)) =>
            Applicative[M].pure[Expr](GBool(basePs.contains(par)))
          case EMapBody(ParMap(basePs, _, _, _)) =>
            Applicative[M].pure[Expr](GBool(basePs.contains(par)))
          case other =>
            s.raiseError(MethodNotDefined("contains", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 1)
                s.raiseError(MethodArgumentNumberMismatch("contains", 1, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          element  <- evalExpr(args(0))
          result   <- contains(baseExpr, element)
          _        <- costAccountingAlg.charge(LOOKUP_COST)
        } yield result
    }

    private[this] val get: Method = new Method() {
      def get(baseExpr: Expr, key: Par): M[Par] =
        baseExpr.exprInstance match {
          case EMapBody(ParMap(basePs, _, _, _)) =>
            Applicative[M].pure[Par](basePs.getOrElse(key, VectorPar()))
          case other =>
            s.raiseError(MethodNotDefined("get", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 1)
                s.raiseError(MethodArgumentNumberMismatch("get", 1, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          key      <- evalExpr(args(0))
          result   <- get(baseExpr, key)
          _        <- costAccountingAlg.charge(LOOKUP_COST)
        } yield result
    }

    private[this] val getOrElse: Method = new Method {
      def getOrElse(baseExpr: Expr, key: Par, default: Par): M[Par] =
        baseExpr.exprInstance match {
          case EMapBody(ParMap(basePs, _, _, _)) =>
            Applicative[M].pure[Par](basePs.getOrElse(key, default))
          case other =>
            s.raiseError(MethodNotDefined("getOrElse", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 2)
                s.raiseError(MethodArgumentNumberMismatch("getOrElse", 2, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          key      <- evalExpr(args(0))
          default  <- evalExpr(args(1))
          result   <- getOrElse(baseExpr, key, default)
          _        <- costAccountingAlg.charge(LOOKUP_COST)
        } yield result
    }

    private[this] val set: Method = new Method() {
      def set(baseExpr: Expr, key: Par, value: Par): M[Par] =
        baseExpr.exprInstance match {
          case EMapBody(ParMap(basePs, _, _, _)) =>
            Applicative[M].pure[Par](ParMap(SortedParMap(basePs + (key -> value))))
          case other =>
            s.raiseError(MethodNotDefined("set", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 2)
                s.raiseError(MethodArgumentNumberMismatch("set", 2, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          key      <- evalExpr(args(0))
          value    <- evalExpr(args(1))
          result   <- set(baseExpr, key, value)
          _        <- costAccountingAlg.charge(ADD_COST)
        } yield result
    }

    private[this] val keys: Method = new Method() {
      def keys(baseExpr: Expr): M[Par] =
        baseExpr.exprInstance match {
          case EMapBody(ParMap(basePs, _, _, _)) =>
            Applicative[M].pure[Par](ParSet(basePs.keys.toSeq))
          case other =>
            s.raiseError(MethodNotDefined("keys", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 0)
                s.raiseError(MethodArgumentNumberMismatch("slice", 2, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          result   <- keys(baseExpr)
        } yield result
    }

    private[this] val size: Method = new Method() {
      def size(baseExpr: Expr): M[Par] =
        baseExpr.exprInstance match {
          case EMapBody(ParMap(basePs, _, _, _)) =>
            Applicative[M].pure[Par](GInt(basePs.size.toLong))
          case ESetBody(ParSet(ps, _, _, _)) =>
            Applicative[M].pure[Par](GInt(ps.size.toLong))
          case other =>
            s.raiseError(MethodNotDefined("size", other.typ))
        }

      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 0)
                s.raiseError(MethodArgumentNumberMismatch("size", 0, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          result   <- size(baseExpr)
        } yield result
    }

    private[this] val length: Method = new Method() {
      def length(baseExpr: Expr): M[Expr] =
        baseExpr.exprInstance match {
          case GString(string) =>
            Applicative[M].pure[Expr](GInt(string.length.toLong))
          case EListBody(EList(ps, _, _, _)) =>
            Applicative[M].pure[Expr](GInt(ps.length.toLong))
          case other =>
            s.raiseError(MethodNotDefined("length", other.typ))
        }
      //TODO(mateusz.gorski): add cost accounting
      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 0)
                s.raiseError(MethodArgumentNumberMismatch("length", 0, args.length))
              else Applicative[M].unit
          baseExpr <- evalSingleExpr(p)
          result   <- length(baseExpr)
        } yield result
    }

    private[this] val slice: Method = new Method() {
      def slice(baseExpr: Expr, from: Int, until: Int): M[Par] =
        baseExpr.exprInstance match {
          case GString(string) =>
            Applicative[M].pure[Par](GString(string.slice(from, until)))
          case EListBody(EList(ps, locallyFree, connectiveUsed, remainder)) =>
            Applicative[M].pure[Par](
              EList(
                ps.slice(from, until),
                locallyFree,
                connectiveUsed,
                remainder
              )
            )
          case GByteArray(bytes) =>
            Applicative[M].pure[Par](GByteArray(bytes.substring(from, until)))
          case other =>
            s.raiseError(MethodNotDefined("slice", other.typ))
        }
      //TODO(mateusz.gorski): add cost accounting
      override def apply(
          p: Par,
          args: Seq[Par]
      )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          _ <- if (args.length != 2)
                s.raiseError(MethodArgumentNumberMismatch("slice", 2, args.length))
              else Applicative[M].unit
          baseExpr   <- evalSingleExpr(p)
          fromArgRaw <- evalToLong(args(0))
          fromArg    <- restrictToInt(fromArgRaw)
          toArgRaw   <- evalToLong(args(1))
          toArg      <- restrictToInt(toArgRaw)
          result     <- slice(baseExpr, fromArg, toArg)
        } yield result
    }

    private val methodTable: Map[String, Method] =
      Map(
        "nth"         -> nth,
        "toByteArray" -> toByteArray,
        "hexToBytes"  -> hexToBytes,
        "toUtf8Bytes" -> toUtf8Bytes,
        "union"       -> union,
        "diff"        -> diff,
        "add"         -> add,
        "delete"      -> delete,
        "contains"    -> contains,
        "get"         -> get,
        "getOrElse"   -> getOrElse,
        "set"         -> set,
        "keys"        -> keys,
        "size"        -> size,
        "length"      -> length,
        "slice"       -> slice
      )

    def evalSingleExpr(
        p: Par
    )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Expr] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty || !p.bundles.isEmpty)
        s.raiseError(
          ReduceError("Error: parallel or non expression found where expression expected.")
        )
      else
        p.exprs match {
          case (e: Expr) +: Nil => evalExprToExpr(e)
          case _ =>
            s.raiseError(ReduceError("Error: Multiple expressions given."))
        }

    def evalToLong(
        p: Par
    )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Long] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty || !p.bundles.isEmpty)
        s.raiseError(
          ReduceError("Error: parallel or non expression found where expression expected.")
        )
      else
        p.exprs match {
          case Expr(GInt(v)) +: Nil =>
            Applicative[M].pure(v)
          case Expr(EVarBody(EVar(v))) +: Nil =>
            for {
              p      <- eval(v)
              intVal <- evalToLong(p)
            } yield intVal
          case (e: Expr) +: Nil =>
            for {
              evaled <- evalExprToExpr(e)
              result <- evaled.exprInstance match {
                         case GInt(v) => Applicative[M].pure(v)
                         case _ =>
                           s.raiseError(
                             ReduceError("Error: expression didn't evaluate to integer.")
                           )
                       }
            } yield result
          case _ =>
            s.raiseError(ReduceError("Error: Integer expected, or unimplemented expression."))
        }

    def evalToBool(
        p: Par
    )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Boolean] =
      if (!p.sends.isEmpty || !p.receives.isEmpty || !p.news.isEmpty || !p.matches.isEmpty || !p.ids.isEmpty || !p.bundles.isEmpty)
        s.raiseError(
          ReduceError("Error: parallel or non expression found where expression expected.")
        )
      else
        p.exprs match {
          case Expr(GBool(b)) +: Nil =>
            Applicative[M].pure(b)
          case Expr(EVarBody(EVar(v))) +: Nil =>
            for {
              p       <- eval(v)
              boolVal <- evalToBool(p)
            } yield boolVal
          case (e: Expr) +: Nil =>
            for {
              evaled <- evalExprToExpr(e)
              result <- evaled.exprInstance match {
                         case GBool(b) => Applicative[M].pure(b)
                         case _ =>
                           s.raiseError(
                             ReduceError("Error: expression didn't evaluate to boolean.")
                           )
                       }
            } yield result
          case _ =>
            s.raiseError(ReduceError("Error: Multiple expressions given."))
        }

    private def restrictToInt(long: Long): M[Int] =
      s.catchNonFatal(Math.toIntExact(long)).adaptError {
        case e: ArithmeticException => ReduceError(s"Integer overflow for value $long")
      }

    private def updateLocallyFree(par: Par): Par = {
      val resultLocallyFree =
        par.sends.foldLeft(BitSet())((acc, send) => acc | send.locallyFree) |
          par.receives.foldLeft(BitSet())((acc, receive) => acc | receive.locallyFree) |
          par.news.foldLeft(BitSet())((acc, newProc) => acc | newProc.locallyFree) |
          par.exprs.foldLeft(BitSet())((acc, expr) => acc | ExprLocallyFree.locallyFree(expr, 0)) |
          par.matches.foldLeft(BitSet())((acc, matchProc) => acc | matchProc.locallyFree) |
          par.bundles.foldLeft(BitSet())((acc, bundleProc) => acc | bundleProc.locallyFree)
      par.copy(locallyFree = resultLocallyFree)
    }

    private def updateLocallyFree(elist: EList): EList = {
      val resultLocallyFree = elist.ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
      elist.copy(locallyFree = resultLocallyFree)
    }

    private def updateLocallyFree(elist: ETuple): ETuple = {
      val resultLocallyFree = elist.ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)
      elist.copy(locallyFree = resultLocallyFree)
    }

    /**
      * Evaluate any top level expressions in @param Par .
      */
    def evalExpr(
        par: Par
    )(implicit env: Env[Par], costAccountingAlg: CostAccountingAlg[M]): M[Par] =
      for {
        evaledExprs <- par.exprs.toList.traverse(expr => evalExprToPar(expr))
        // Note: the locallyFree cache in par could now be invalid, but given
        // that locallyFree is for use in the matcher, and the matcher uses
        // substitution, it will resolve in that case. AlwaysEqual makes sure
        // that this isn't an issue in the rest of cases.
        result = evaledExprs.foldLeft(par.copy(exprs = Vector())) { (acc, newPar) =>
          acc ++ newPar
        }
      } yield result
  }
}
