package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.{Parallel, Eval => _}
import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.serialization.implicits._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoTuplespace
import coop.rchain.rholang.interpreter.RholangAndScalaDispatcher.RhoDispatch
import coop.rchain.rholang.interpreter.Substitute.{charge => _, _}
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.matcher.SpatialMatcher.spatialMatchResult
import coop.rchain.rspace.util.unpackOptionWithPeek
import coop.rchain.shared.Serialize
import monix.eval.Coeval
import scalapb.GeneratedMessage

import scala.collection.SortedSet
import scala.collection.immutable.BitSet
import scala.util.Try

/** Reduce is the interface for evaluating Rholang expressions.
  *
  * @tparam M The kind of Monad used for evaluation.
  */
trait Reduce[M[_]] {

  def eval(par: Par)(implicit env: Env[Par], rand: Blake2b512Random): M[Unit]

  final def inj(par: Par)(implicit rand: Blake2b512Random): M[Unit] =
    eval(par)(Env[Par](), rand)

}

class DebruijnInterpreter[M[_]: Sync: Parallel: _cost](
    space: RhoTuplespace[M],
    dispatcher: => RhoDispatch[M],
    urnMap: Map[String, Par],
    mergeChs: Ref[M, Set[Par]]
) extends Reduce[M] {

  type Application =
    Option[
      (TaggedContinuation, Seq[(Par, ListParWithRandom, ListParWithRandom, Boolean)], Boolean)
    ]

  /**
    * Materialize a send in the store, optionally returning the matched continuation.
    *
    * @param chan  The channel on which data is being sent.
    * @param data  The par objects holding the processes being sent.
    * @param persistent  True if the write should remain in the tuplespace indefinitely.
    */
  private def produce(
      chan: Par,
      data: ListParWithRandom,
      persistent: Boolean
  ): M[Unit] =
    updateMergeableChannels(chan) *>
      space.produce(chan, data, persist = persistent) >>= { produceResult =>
      continue(
        unpackOptionWithPeek(produceResult),
        produce(chan, data, persistent),
        persistent
      )
    }

  /**
    * Materialize a send in the store, optionally returning the matched continuation.
    *
    * @param binds  A Seq of pattern, channel pairs. Each pattern is a Seq[Par].
    *               The Seq is for arity matching, and each term in the Seq is a name pattern.
    * @param body  A Par object which will be run in the envirnoment resulting from the match.
    */
  private def consume(
      binds: Seq[(BindPattern, Par)],
      body: ParWithRandom,
      persistent: Boolean,
      peek: Boolean
  ): M[Unit] = {
    val (patterns: Seq[BindPattern], sources: Seq[Par]) = binds.unzip

    sources.toList.traverse(updateMergeableChannels) *>
      space.consume(
        sources.toList,
        patterns.toList,
        TaggedContinuation(ParBody(body)),
        persist = persistent,
        if (peek) SortedSet(sources.indices: _*) else SortedSet.empty[Int]
      ) >>= { consumeResult =>
      continue(
        unpackOptionWithPeek(consumeResult),
        consume(binds, body, persistent, peek),
        persistent
      )
    }
  }

  private[this] def continue(res: Application, repeatOp: M[Unit], persistent: Boolean): M[Unit] =
    res match {
      case Some((continuation, dataList, _)) if persistent =>
        dispatchAndRun(continuation, dataList)(
          repeatOp
        )
      case Some((continuation, dataList, peek)) if peek =>
        dispatchAndRun(continuation, dataList)(
          producePeeks(dataList): _*
        )
      case Some((continuation, dataList, _)) =>
        dispatch(continuation, dataList)
      case None => Sync[M].unit
    }

  private[this] def dispatchAndRun(
      continuation: TaggedContinuation,
      dataList: Seq[(Par, ListParWithRandom, ListParWithRandom, Boolean)]
  )(ops: M[Unit]*): M[Unit] =
    // Collect errors from all parallel execution paths (pars)
    parTraverseSafe(dispatch(continuation, dataList) +: ops.toVector)(identity)

  private[this] def dispatch(
      continuation: TaggedContinuation,
      dataList: Seq[(Par, ListParWithRandom, ListParWithRandom, Boolean)]
  ): M[Unit] = dispatcher.dispatch(
    continuation,
    dataList.map(_._2)
  )

  private[this] def producePeeks(
      dataList: Seq[(Par, ListParWithRandom, ListParWithRandom, Boolean)]
  ): Seq[M[Unit]] =
    dataList
      .withFilter {
        case (_, _, _, persist) => !persist
      }
      .map {
        case (chan, _, removedData, _) =>
          produce(chan, removedData, persistent = false)
      }

  /* Collect mergeable channels */

  private def updateMergeableChannels(chan: Par) = Sync[M].defer {
    val isMergeable = isMergeableChannel(chan)

    mergeChs.update(_ + chan).whenA(isMergeable)
  }

  private def isMergeableChannel(chan: Par) = {
    val tupleElms     = chan.exprs.flatMap(y => y.getETupleBody.ps)
    val mergeStr: Par = GString("__MERGEABLE__")
    tupleElms.headOption.contains(mergeStr)
  }

  /**
    * The evaluation of a Par is at the mercy of `Parallel` instance passed to `DebruijnInterpreter`. Therefore, if a
    * particular concurrency semantics is desired, it should be achieved by modifying the `Parallel` instance, not by
    * modifying this method.
    */
  override def eval(par: Par)(
      implicit env: Env[Par],
      rand: Blake2b512Random
  ): M[Unit] = {
    val terms: Seq[GeneratedMessage] = Seq(
      par.sends,
      par.receives,
      par.news,
      par.matches,
      par.bundles,
      par.exprs.filter { expr =>
        expr.exprInstance match {
          case _: EVarBody    => true
          case _: EMethodBody => true
          case _              => false
        }
      }
    ).filter(_.nonEmpty).flatten

    def split(id: Int): Blake2b512Random =
      if (terms.size == 1) rand
      else if (terms.size > 256) rand.splitShort(id.toShort)
      else rand.splitByte(id.toByte)

    // Term split size is limited to half of Int16 because other half is for injecting
    // things to system deploys through NormalizerEnv
    val termSplitLimit = Short.MaxValue
    if (terms.size > termSplitLimit)
      ReduceError(
        s"The number of terms in the Par is ${terms.size}, which exceeds the limit of ${termSplitLimit}."
      ).raiseError[M, Unit]
    else {

      // Collect errors from all parallel execution paths (pars)
      parTraverseSafe(terms.zipWithIndex.toVector) {
        case (term, index) =>
          eval(term)(env, split(index))
      }
    }
  }

  private def parTraverseSafe[A](xs: Vector[A])(op: A => M[Unit]): M[Unit] =
    xs.parTraverse(op(_).map(_ => none[Throwable]).handleError(_.some))
      .map(_.flattenOption)
      .flatMap(aggregateEvaluatorErrors)

  private def aggregateEvaluatorErrors(errors: Vector[Throwable]) = errors match {
    // No errors
    case Vector() => ().pure[M]

    // Out Of Phlogiston error is always single
    // - if one execution path is out of phlo, the whole evaluation is also
    case errList if errList.contains(OutOfPhlogistonsError) =>
      OutOfPhlogistonsError.raiseError[M, Unit]

    // Rethrow single error
    case Vector(ex) =>
      ex.raiseError[M, Unit]

    // Collect errors from parallel execution
    case errList =>
      val (interpErrs, errs) = errList.foldLeft((Vector[InterpreterError](), Vector[Throwable]())) {
        // Concat nested errors
        case ((ipErr1, err1), AggregateError(ipErr2, err2)) => (ipErr1 ++ ipErr2, err1 ++ err2)
        case ((ipErr, err), ex: InterpreterError)           => (ipErr :+ ex, err)
        case ((ipErr, err), ex: Throwable)                  => (ipErr, err :+ ex)
      }
      AggregateError(interpErrs, errs).raiseError[M, Unit]
  }

  private def eval(
      term: GeneratedMessage
  )(implicit env: Env[Par], rand: Blake2b512Random): M[Unit] =
    term match {
      case term: Send    => eval(term)
      case term: Receive => eval(term)
      case term: New     => eval(term)
      case term: Match   => eval(term)
      case term: Bundle  => eval(term)
      case term: Expr =>
        term.exprInstance match {
          case e: EVarBody    => eval(e.value.v) >>= eval
          case e: EMethodBody => evalExprToPar(e) >>= eval
          case other          => BugFoundError(s"Undefined term: \n $other").raiseError[M, Unit]
        }
      case other => BugFoundError(s"Undefined term: \n $other").raiseError[M, Unit]
    }

  /** Algorithm as follows:
    *
    * 1. Fully evaluate the channel in given environment.
    * 2. Substitute any variable references in the channel so that it can be
    *    correctly used as a key in the tuple space.
    * 3. Evaluate any top level expressions in the data being sent.
    * 4. Call produce
    *
    * @param send An output process
    * @param env An execution context
    *
    */
  private def eval(send: Send)(
      implicit env: Env[Par],
      rand: Blake2b512Random
  ): M[Unit] =
    for {
      _        <- charge[M](SEND_EVAL_COST)
      evalChan <- evalExpr(send.chan)
      subChan  <- substituteAndCharge[Par, M](evalChan, depth = 0, env)
      unbundled <- subChan.singleBundle() match {
                    case Some(value) =>
                      if (!value.writeFlag)
                        ReduceError("Trying to send on non-writeable channel.").raiseError[M, Par]
                      else value.body.pure[M]
                    case None => subChan.pure[M]
                  }
      data      <- send.data.toList.traverse(evalExpr)
      substData <- data.traverse(substituteAndCharge[Par, M](_, depth = 0, env))
      _         <- produce(unbundled, ListParWithRandom(substData, rand), send.persistent)
    } yield ()

  private def eval(receive: Receive)(
      implicit env: Env[Par],
      rand: Blake2b512Random
  ): M[Unit] =
    for {
      _ <- charge[M](RECEIVE_EVAL_COST)
      binds <- receive.binds.toList.traverse { rb =>
                for {
                  q <- unbundleReceive(rb)
                  substPatterns <- rb.patterns.toList
                                    .traverse(substituteAndCharge[Par, M](_, depth = 1, env))
                } yield (BindPattern(substPatterns, rb.remainder, rb.freeCount), q)
              }
      // TODO: Allow for the environment to be stored with the body in the Tuplespace
      substBody <- substituteNoSortAndCharge[Par, M](
                    receive.body,
                    depth = 0,
                    env.shift(receive.bindCount)
                  )
      _ <- consume(binds, ParWithRandom(substBody, rand), receive.persistent, receive.peek)
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
    */
  private def eval(
      valproc: Var
  )(implicit env: Env[Par]): M[Par] =
    charge[M](VAR_EVAL_COST) >> {
      valproc.varInstance match {
        case BoundVar(level) =>
          env.get(level).liftTo[M](ReduceError("Unbound variable: " + level + " in " + env.envMap))
        case Wildcard(_) =>
          ReduceError("Unbound variable: attempting to evaluate a pattern").raiseError[M, Par]
        case FreeVar(_) =>
          ReduceError("Unbound variable: attempting to evaluate a pattern").raiseError[M, Par]
        case VarInstance.Empty =>
          ReduceError("Impossible var instance EMPTY").raiseError[M, Par]
      }
    }

  private def eval(mat: Match)(
      implicit env: Env[Par],
      rand: Blake2b512Random
  ): M[Unit] = {

    def addToEnv(env: Env[Par], freeMap: Map[Int, Par], freeCount: Int): Env[Par] =
      Range(0, freeCount).foldLeft(env)((acc, e) => acc.put(freeMap.getOrElse(e, Par())))

    def firstMatch(target: Par, cases: Seq[MatchCase])(implicit env: Env[Par]): M[Unit] = {
      def firstMatchM(
          state: (Par, Seq[MatchCase])
      ): M[Either[(Par, Seq[MatchCase]), Unit]] = {
        val (target, cases) = state
        cases match {
          case Nil => ().asRight[(Par, Seq[MatchCase])].pure[M]
          case singleCase +: caseRem =>
            for {
              pattern     <- substituteAndCharge[Par, M](singleCase.pattern, depth = 1, env)
              matchResult <- spatialMatchResult[M](target, pattern)
              res <- matchResult match {
                      case None =>
                        (target, caseRem).asLeft[Unit].pure[M]
                      case Some((freeMap, _)) =>
                        eval(singleCase.source)(
                          addToEnv(env, freeMap, singleCase.freeCount),
                          implicitly
                        ).map(_.asRight[(Par, Seq[MatchCase])])
                    }
            } yield res
        }
      }
      (target, cases).tailRecM(firstMatchM)
    }

    for {
      _            <- charge[M](MATCH_EVAL_COST)
      evaledTarget <- evalExpr(mat.target)
      // TODO(kyle): Make the matcher accept an environment, instead of substituting it.
      substTarget <- substituteAndCharge[Par, M](evaledTarget, depth = 0, env)
      _           <- firstMatch(substTarget, mat.cases)
    } yield ()
  }

  /**
    * Adds neu.bindCount new GPrivate from UUID's to the environment and then
    * proceeds to evaluate the body.
    */
  // TODO: Eliminate variable shadowing
  private def eval(
      neu: New
  )(implicit env: Env[Par], rand: Blake2b512Random): M[Unit] = {

    def alloc(count: Int, urns: Seq[String]): M[Env[Par]] = {
      val simpleNews = (0 until (count - urns.size)).toList.foldLeft(env) { (_env, _) =>
        val addr: Par = GPrivate(ByteString.copyFrom(rand.next()))
        _env.put(addr)
      }

      def normalizerBugFound(urn: String) =
        BugFoundError(
          s"No value set for `$urn`. This is a bug in the normalizer or on the path from it."
        )

      def addUrn(newEnv: Env[Par], urn: String): Either[InterpreterError, Env[Par]] =
        if (!urnMap.contains(urn))
          /** TODO: Injections (from normalizer) are not used currently, see [[NormalizerEnv]]. */
          // If `urn` can't be found in `urnMap`, it must be referencing an injection
          neu.injections
            .get(urn)
            .map {
              case RhoType.Unforgeable(GUnforgeable(unfInstance)) if unfInstance.isDefined =>
                newEnv.put(unfInstance).asRight[InterpreterError]

              case RhoType.Expression(Expr(exprInstance)) if exprInstance.isDefined =>
                newEnv.put(exprInstance).asRight[InterpreterError]

              case _ =>
                BugFoundError("Invalid injection.").asLeft[Env[Par]]
            }
            .getOrElse(normalizerBugFound(urn).asLeft[Env[Par]])
        else
          urnMap.get(urn).map(newEnv.put).toRight(left = ReduceError(s"Unknown urn for new: $urn"))

      urns.toList.foldM(simpleNews)(addUrn).fold(_.raiseError[M, Env[Par]], _.pure[M])
    }

    charge[M](newBindingsCost(neu.bindCount)) >>
      alloc(neu.bindCount, neu.uri).flatMap(eval(neu.p)(_, rand))
  }

  private[this] def unbundleReceive(rb: ReceiveBind)(implicit env: Env[Par]): M[Par] =
    for {
      evalSrc <- evalExpr(rb.source)
      subst   <- substituteAndCharge[Par, M](evalSrc, depth = 0, env)
      // Check if we try to read from bundled channel
      unbndl <- subst.singleBundle() match {
                 case Some(value) =>
                   if (!value.readFlag)
                     ReduceError("Trying to read from non-readable channel.").raiseError[M, Par]
                   else value.body.pure[M]
                 case None => subst.pure[M]
               }
    } yield unbndl

  private def eval(
      bundle: Bundle
  )(implicit env: Env[Par], rand: Blake2b512Random): M[Unit] =
    eval(bundle.body)

  private[rholang] def evalExprToPar(expr: Expr)(implicit env: Env[Par]): M[Par] =
    expr.exprInstance match {
      case EVarBody(EVar(v)) =>
        for {
          p       <- eval(v)
          evaledP <- evalExpr(p)
        } yield evaledP
      case EMethodBody(EMethod(method, target, arguments, _, _)) => {
        for {
          _            <- charge[M](METHOD_CALL_COST)
          evaledTarget <- evalExpr(target)
          evaledArgs   <- arguments.toList.traverse(evalExpr)
          resultPar <- methodTable
                        .get(method)
                        .liftTo[M](ReduceError("Unimplemented method: " + method))
                        .flatMap(_(evaledTarget, evaledArgs))
        } yield resultPar
      }
      case _ => evalExprToExpr(expr).map(fromExpr(_)(identity))
    }

  private def evalExprToExpr(expr: Expr)(implicit env: Env[Par]): M[Expr] = {
    Sync[M].defer {
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
                     case (GBool(b1), GBool(b2))     => GBool(relopb(b1, b2)).pure[M]
                     case (GInt(i1), GInt(i2))       => GBool(relopi(i1, i2)).pure[M]
                     case (GString(s1), GString(s2)) => GBool(relops(s1, s2)).pure[M]
                     case _ =>
                       ReduceError("Unexpected compare: " + v1 + " vs. " + v2).raiseError[M, GBool]
                   }
        } yield result

      expr.exprInstance match {
        case x: GBool          => (x: Expr).pure[M]
        case x: GInt           => (x: Expr).pure[M]
        case x: GString        => (x: Expr).pure[M]
        case x: GUri           => (x: Expr).pure[M]
        case x: GByteArray     => (x: Expr).pure[M]
        case ENotBody(ENot(p)) => evalToBool(p).map(b => GBool(!b))
        case ENegBody(ENeg(p)) => evalToLong(p).map(v => GInt(-v))
        case EMultBody(EMult(p1, p2)) =>
          for {
            v1 <- evalToLong(p1)
            v2 <- evalToLong(p2)
            _  <- charge[M](MULTIPLICATION_COST)
          } yield GInt(v1 * v2)

        case EDivBody(EDiv(p1, p2)) =>
          for {
            v1 <- evalToLong(p1)
            v2 <- evalToLong(p2)
            _  <- charge[M](DIVISION_COST)
          } yield GInt(v1 / v2)

        case EModBody(EMod(p1, p2)) =>
          for {
            v1 <- evalToLong(p1)
            v2 <- evalToLong(p2)
            _  <- charge[M](MODULO_COST)
          } yield GInt(v1 % v2)

        case EPlusBody(EPlus(p1, p2)) =>
          for {
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (GInt(lhs), GInt(rhs)) =>
                         charge[M](SUM_COST) >> Expr(GInt(lhs + rhs)).pure[M]
                       case (lhs: ESetBody, rhs) =>
                         for {
                           _         <- charge[M](OP_CALL_COST)
                           resultPar <- add(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (_: GInt, other) =>
                         OperatorExpectedError("+", "Int", other.typ).raiseError[M, Expr]
                       case (other, _) => OperatorNotDefined("+", other.typ).raiseError[M, Expr]
                     }
          } yield result

        case EMinusBody(EMinus(p1, p2)) =>
          for {
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (GInt(lhs), GInt(rhs)) =>
                         charge[M](SUBTRACTION_COST) >> Expr(GInt(lhs - rhs)).pure[M]
                       case (lhs: EMapBody, rhs) =>
                         for {
                           _         <- charge[M](OP_CALL_COST)
                           resultPar <- delete(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (lhs: ESetBody, rhs) =>
                         for {
                           _         <- charge[M](OP_CALL_COST)
                           resultPar <- delete(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (_: GInt, other) =>
                         OperatorExpectedError("-", "Int", other.typ).raiseError[M, Expr]
                       case (other, _) =>
                         OperatorNotDefined("-", other.typ).raiseError[M, Expr]
                     }
          } yield result

        case ELtBody(ELt(p1, p2)) =>
          charge[M](COMPARISON_COST) >> relop(p1, p2, (_ < _), (_ < _), (_ < _))

        case ELteBody(ELte(p1, p2)) =>
          charge[M](COMPARISON_COST) >> relop(p1, p2, (_ <= _), (_ <= _), (_ <= _))

        case EGtBody(EGt(p1, p2)) =>
          charge[M](COMPARISON_COST) >> relop(p1, p2, (_ > _), (_ > _), (_ > _))

        case EGteBody(EGte(p1, p2)) =>
          charge[M](COMPARISON_COST) >> relop(p1, p2, (_ >= _), (_ >= _), (_ >= _))

        case EEqBody(EEq(p1, p2)) =>
          for {
            v1 <- evalExpr(p1)
            v2 <- evalExpr(p2)
            // TODO: build an equality operator that takes in an environment.
            sv1 <- substituteAndCharge[Par, M](v1, depth = 0, env)
            sv2 <- substituteAndCharge[Par, M](v2, depth = 0, env)
            _   <- charge[M](equalityCheckCost(sv1, sv2))
          } yield GBool(sv1 == sv2)

        case ENeqBody(ENeq(p1, p2)) =>
          for {
            v1  <- evalExpr(p1)
            v2  <- evalExpr(p2)
            sv1 <- substituteAndCharge[Par, M](v1, depth = 0, env)
            sv2 <- substituteAndCharge[Par, M](v2, depth = 0, env)
            _   <- charge[M](equalityCheckCost(sv1, sv2))
          } yield GBool(sv1 != sv2)

        case EAndBody(EAnd(p1, p2)) =>
          for {
            b1 <- evalToBool(p1)
            b2 <- evalToBool(p2)
            _  <- charge[M](BOOLEAN_AND_COST)
          } yield GBool(b1 && b2)

        case EOrBody(EOr(p1, p2)) =>
          for {
            b1 <- evalToBool(p1)
            b2 <- evalToBool(p2)
            _  <- charge[M](BOOLEAN_OR_COST)
          } yield GBool(b1 || b2)

        case EMatchesBody(EMatches(target, pattern)) =>
          for {
            evaledTarget <- evalExpr(target)
            substTarget  <- substituteAndCharge[Par, M](evaledTarget, depth = 0, env)
            substPattern <- substituteAndCharge[Par, M](pattern, depth = 1, env)
            matchResult  <- spatialMatchResult[M](substTarget, substPattern)
          } yield GBool(matchResult.isDefined)

        case EPercentPercentBody(EPercentPercent(p1, p2)) =>
          def evalToStringPair(keyExpr: Expr, valueExpr: Expr): M[(String, String)] =
            (keyExpr.exprInstance, valueExpr.exprInstance) match {
              case (GString(keyString), GString(valueString)) =>
                (keyString -> valueString).pure[M]
              case (GString(keyString), GInt(valueInt)) =>
                (keyString -> valueInt.toString).pure[M]
              case (GString(keyString), GBool(valueBool)) =>
                (keyString -> valueBool.toString).pure[M]
              case (GString(keyString), GUri(uri)) =>
                (keyString -> uri).pure[M]
              // TODO: Add cases for other ground terms as well? Maybe it would be better
              // to implement cats.Show for all ground terms.
              case (_: GString, value) =>
                ReduceError(s"Error: interpolation doesn't support ${value.typ}")
                  .raiseError[M, (String, String)]
              case _ =>
                ReduceError("Error: interpolation Map should only contain String keys")
                  .raiseError[M, (String, String)]
            }
          @SuppressWarnings(
            Array("org.wartremover.warts.Var", "org.wartremover.warts.NonUnitStatements")
          )
          // TODO consider replacing while loop with tailrec recursion
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
            _  <- charge[M](OP_CALL_COST)
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (GString(lhs), EMapBody(ParMap(rhs, _, _, _))) =>
                         if (lhs.nonEmpty || rhs.nonEmpty) {
                           for {
                             result <- rhs.toList
                                        .traverse {
                                          case (k, v) =>
                                            for {
                                              keyExpr   <- evalSingleExpr(k)
                                              valueExpr <- evalSingleExpr(v)
                                              result    <- evalToStringPair(keyExpr, valueExpr)
                                            } yield result
                                        }
                                        .map(
                                          keyValuePairs => GString(interpolate(lhs, keyValuePairs))
                                        )
                             _ <- charge[M](interpolateCost(lhs.length, rhs.size))
                           } yield result
                         } else GString(lhs).pure[M]
                       case (_: GString, other) =>
                         OperatorExpectedError("%%", "Map", other.typ).raiseError[M, GString]
                       case (other, _) =>
                         OperatorNotDefined("%%", other.typ).raiseError[M, GString]
                     }
          } yield result

        case EPlusPlusBody(EPlusPlus(p1, p2)) =>
          for {
            _  <- charge[M](OP_CALL_COST)
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (GString(lhs), GString(rhs)) =>
                         charge[M](stringAppendCost(lhs.length, rhs.length)) >>
                           Expr(GString(lhs + rhs)).pure[M]
                       case (GByteArray(lhs), GByteArray(rhs)) =>
                         charge[M](byteArrayAppendCost(lhs)) >>
                           Expr(GByteArray(lhs.concat(rhs))).pure[M]
                       case (EListBody(lhs), EListBody(rhs)) =>
                         charge[M](listAppendCost(rhs.ps.toVector)) >>
                           Expr(
                             EListBody(
                               EList(
                                 lhs.ps ++ rhs.ps,
                                 lhs.locallyFree union rhs.locallyFree,
                                 lhs.connectiveUsed || rhs.connectiveUsed
                               )
                             )
                           ).pure[M]
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
                         OperatorExpectedError("++", "String", other.typ).raiseError[M, Expr]
                       case (_: EListBody, other) =>
                         OperatorExpectedError("++", "List", other.typ).raiseError[M, Expr]
                       case (_: EMapBody, other) =>
                         OperatorExpectedError("++", "Map", other.typ).raiseError[M, Expr]
                       case (_: ESetBody, other) =>
                         OperatorExpectedError("++", "Set", other.typ).raiseError[M, Expr]
                       case (other, _) => OperatorNotDefined("++", other.typ).raiseError[M, Expr]
                     }
          } yield result

        case EMinusMinusBody(EMinusMinus(p1, p2)) =>
          for {
            _  <- charge[M](OP_CALL_COST)
            v1 <- evalSingleExpr(p1)
            v2 <- evalSingleExpr(p2)
            result <- (v1.exprInstance, v2.exprInstance) match {
                       case (lhs: ESetBody, rhs: ESetBody) =>
                         for {
                           resultPar <- diff(lhs, List[Par](rhs))
                           resultExp <- evalSingleExpr(resultPar)
                         } yield resultExp
                       case (_: ESetBody, other) =>
                         OperatorExpectedError("--", "Set", other.typ).raiseError[M, Expr]
                       case (other, _) => OperatorNotDefined("--", other.typ).raiseError[M, Expr]
                     }
          } yield result

        case EVarBody(EVar(v)) =>
          for {
            p       <- eval(v)
            exprVal <- evalSingleExpr(p)
          } yield exprVal

        case EListBody(el) =>
          for {
            evaledPs  <- el.ps.toList.traverse(evalExpr)
            updatedPs = evaledPs.map(updateLocallyFree)
          } yield updateLocallyFree(EList(updatedPs, el.locallyFree, el.connectiveUsed))

        case ETupleBody(el) =>
          for {
            evaledPs  <- el.ps.toList.traverse(evalExpr)
            updatedPs = evaledPs.map(updateLocallyFree)
          } yield updateLocallyFree(ETuple(updatedPs, el.locallyFree, el.connectiveUsed))

        case ESetBody(set) =>
          for {
            evaledPs  <- set.ps.sortedPars.traverse(evalExpr)
            updatedPs = evaledPs.map(updateLocallyFree)
          } yield set.copy(ps = SortedParHashSet(updatedPs))

        case EMapBody(map) =>
          for {
            evaledPs <- map.ps.sortedList.traverse {
                         case (key, value) =>
                           for {
                             eKey   <- evalExpr(key).map(updateLocallyFree)
                             eValue <- evalExpr(value).map(updateLocallyFree)
                           } yield (eKey, eValue)
                       }
          } yield map.copy(ps = SortedParMap(evaledPs))

        case EMethodBody(EMethod(method, target, arguments, _, _)) =>
          for {
            _            <- charge[M](METHOD_CALL_COST)
            evaledTarget <- evalExpr(target)
            evaledArgs   <- arguments.toList.traverse(evalExpr)
            resultPar <- methodTable
                          .get(method)
                          .liftTo[M](ReduceError("Unimplemented method: " + method))
                          .flatMap(_(evaledTarget, evaledArgs))
            resultExpr <- evalSingleExpr(resultPar)
          } yield resultExpr
        case _ => ReduceError("Unimplemented expression: " + expr).raiseError[M, Expr]
      }
    }
  }

  private abstract class Method() {
    def apply(p: Par, args: Seq[Par])(
        implicit env: Env[Par]
    ): M[Par]
  }

  private[this] val nth: Method = new Method() {

    def localNth(ps: Seq[Par], nth: Int): Either[ReduceError, Par] =
      if (ps.isDefinedAt(nth)) ps(nth).asRight[ReduceError]
      else ReduceError("Error: index out of bound: " + nth).asLeft[Par]

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      if (args.length != 1)
        errors.MethodArgumentNumberMismatch("nth", 1, args.length).raiseError[M, Par]
      else {
        for {
          _      <- charge[M](NTH_METHOD_CALL_COST)
          nthRaw <- evalToLong(args.head)
          nth    <- restrictToInt(nthRaw)
          v      <- evalSingleExpr(p)
          result <- v.exprInstance match {
                     case EListBody(EList(ps, _, _, _)) =>
                       Sync[M].fromEither(localNth(ps, nth))
                     case ETupleBody(ETuple(ps, _, _)) =>
                       Sync[M].fromEither(localNth(ps, nth))
                     case GByteArray(bs) =>
                       Sync[M].fromEither(if (0 <= nth && nth < bs.size) {
                         val b      = bs.byteAt(nth) & 0xff // convert to unsigned
                         val p: Par = Expr(GInt(b.toLong))
                         p.asRight[ReduceError]
                       } else ReduceError("Error: index out of bound: " + nth).asLeft[Par])
                     case _ =>
                       ReduceError("Error: nth applied to something that wasn't a list or tuple.")
                         .raiseError[M, Par]
                   }
        } yield result
      }
  }

  private[this] val toByteArray: Method = new Method() {

    def serialize(p: Par): Either[ReduceError, Array[Byte]] =
      Either
        .fromTry(Try(Serialize[Par].encode(p).toArray))
        .leftMap(th => ReduceError(s"Error: exception thrown when serializing $p." + th.getMessage))

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      if (args.nonEmpty)
        MethodArgumentNumberMismatch("toByteArray", 0, args.length).raiseError[M, Par]
      else {
        for {
          exprEvaled <- evalExpr(p)
          exprSubst  <- substituteAndCharge[Par, M](exprEvaled, depth = 0, env)
          _          <- charge[M](toByteArrayCost(exprSubst))
          ba         <- Sync[M].fromEither(serialize(exprSubst))
        } yield Expr(GByteArray(ByteString.copyFrom(ba)))
      }
  }

  private[this] val hexToBytes: Method = new Method() {

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      if (args.nonEmpty)
        MethodArgumentNumberMismatch("hexToBytes", 0, args.length).raiseError[M, Par]
      else {
        p.singleExpr() match {
          case Some(Expr(GString(encoded))) =>
            for {
              _ <- charge[M](hexToBytesCost(encoded))
              res <- Sync[M]
                      .delay(ByteString.copyFrom(Base16.unsafeDecode(encoded)))
                      .handleErrorWith { ex =>
                        ReduceError(
                          s"Error: exception was thrown when decoding input string to hexadecimal: ${ex.getMessage}"
                        ).raiseError[M, ByteString]
                      }
                      .map(ba => Expr(GByteArray(ba)): Par)
            } yield res
          case Some(Expr(other)) =>
            MethodNotDefined("hexToBytes", other.typ).raiseError[M, Par]
          case None =>
            ReduceError("Error: Method can only be called on singular expressions.")
              .raiseError[M, Par]
        }
      }
  }

  private[this] val bytesToHex: Method = new Method() {

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      if (args.nonEmpty)
        MethodArgumentNumberMismatch("bytesToHex", 0, args.length).raiseError[M, Par]
      else {
        p.singleExpr() match {
          case Some(Expr(GByteArray(bytes))) =>
            for {
              _ <- charge[M](bytesToHexCost(bytes.toByteArray))
              res <- Sync[M]
                      .delay(Base16.encode(bytes.toByteArray))
                      .handleErrorWith { ex =>
                        ReduceError(
                          s"Error: exception was thrown when encoding input byte array to hexadecimal string: ${ex.getMessage}"
                        ).raiseError[M, String]
                      }
                      .map(str => Expr(GString(str)): Par)
            } yield res
          case Some(Expr(other)) =>
            MethodNotDefined("bytesToHex", other.typ).raiseError[M, Par]
          case None =>
            ReduceError("Error: Method can only be called on singular expressions.")
              .raiseError[M, Par]
        }
      }
  }

  private[this] val toUtf8Bytes: Method = new Method() {

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      if (args.nonEmpty)
        MethodArgumentNumberMismatch("toUtf8Bytes", 0, args.length).raiseError[M, Par]
      else {
        p.singleExpr() match {
          case Some(Expr(GString(utf8string))) =>
            charge[M](hexToBytesCost(utf8string)) >>
              (GByteArray(ByteString.copyFrom(utf8string.getBytes("UTF-8"))): Par).pure[M]
          case Some(Expr(other)) =>
            MethodNotDefined("toUtf8Bytes", other.typ).raiseError[M, Par]
          case None =>
            ReduceError("Error: Method can only be called on singular expressions.")
              .raiseError[M, Par]
        }
      }
  }

  private[this] val union: Method = new Method() {

    def locallyFreeUnion(base: Coeval[BitSet], other: Coeval[BitSet]): Coeval[BitSet] =
      base.flatMap(b => other.map(o => b | o))

    def union(baseExpr: Expr, otherExpr: Expr): M[Expr] =
      (baseExpr.exprInstance, otherExpr.exprInstance) match {
        case (
            ESetBody(base @ ParSet(basePs, _, _, _)),
            ESetBody(other @ ParSet(otherPs, _, _, _))
            ) =>
          charge[M](unionCost(otherPs.size)) >>
            Expr(
              ESetBody(
                ParSet(
                  basePs.union(otherPs.sortedPars.toSet),
                  base.connectiveUsed || other.connectiveUsed,
                  locallyFreeUnion(base.locallyFree, other.locallyFree),
                  None
                )
              )
            ).pure[M]
        case (
            EMapBody(base @ ParMap(baseMap, _, _, _)),
            EMapBody(other @ ParMap(otherMap, _, _, _))
            ) =>
          charge[M](unionCost(otherMap.size)) >>
            Expr(
              EMapBody(
                ParMap(
                  (baseMap ++ otherMap).toSeq,
                  base.connectiveUsed || other.connectiveUsed,
                  locallyFreeUnion(base.locallyFree, other.locallyFree),
                  None
                )
              )
            ).pure[M]

        case (other, _) => MethodNotDefined("union", other.typ).raiseError[M, Expr]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("union", 1, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 1)
        baseExpr  <- evalSingleExpr(p)
        otherExpr <- evalSingleExpr(args.head)
        result    <- union(baseExpr, otherExpr)
      } yield result
  }

  private[this] val diff: Method = new Method() {

    def diff(baseExpr: Expr, otherExpr: Expr): M[Expr] =
      (baseExpr.exprInstance, otherExpr.exprInstance) match {
        case (ESetBody(ParSet(basePs, _, _, _)), ESetBody(ParSet(otherPs, _, _, _))) =>
          // diff is implemented in terms of foldLeft that at each step
          // removes one element from the collection.
          charge[M](diffCost(otherPs.size)) >>
            Expr(ESetBody(ParSet(basePs.sortedPars.toSet.diff(otherPs.sortedPars.toSet).toSeq)))
              .pure[M]
        case (EMapBody(ParMap(basePs, _, _, _)), EMapBody(ParMap(otherPs, _, _, _))) =>
          charge[M](diffCost(otherPs.size)) >>
            Expr(EMapBody(ParMap(basePs -- otherPs.keys))).pure[M]
        case (other, _) =>
          MethodNotDefined("diff", other.typ).raiseError[M, Expr]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("diff", 1, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 1)
        baseExpr  <- evalSingleExpr(p)
        otherExpr <- evalSingleExpr(args.head)
        result    <- diff(baseExpr, otherExpr)
      } yield result
  }

  private[this] val add: Method = new Method() {

    def add(baseExpr: Expr, par: Par): M[Expr] =
      baseExpr.exprInstance match {
        case ESetBody(base @ ParSet(basePs, _, _, _)) =>
          Expr(
            ESetBody(
              ParSet(
                basePs + par,
                base.connectiveUsed || par.connectiveUsed,
                base.locallyFree.map(b => b | par.locallyFree),
                None
              )
            )
          ).pure[M]
        case other => MethodNotDefined("add", other.typ).raiseError[M, Expr]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("add", 1, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 1)
        baseExpr <- evalSingleExpr(p)
        element  <- evalExpr(args.head)
        _        <- charge[M](ADD_COST)
        result   <- add(baseExpr, element)
      } yield result
  }

  private[this] val delete: Method = new Method() {

    def delete(baseExpr: Expr, par: Par): M[Expr] =
      baseExpr.exprInstance match {
        case ESetBody(base @ ParSet(basePs, _, _, _)) =>
          Expr(
            ESetBody(
              ParSet(
                basePs - par,
                base.connectiveUsed || par.connectiveUsed,
                base.locallyFree.map(b => b | par.locallyFree),
                None
              )
            )
          ).pure[M]
        case EMapBody(base @ ParMap(basePs, _, _, _)) =>
          Expr(
            EMapBody(
              ParMap(
                basePs - par,
                base.connectiveUsed || par.connectiveUsed,
                base.locallyFree.map(b => b | par.locallyFree),
                None
              )
            )
          ).pure[M]
        case other =>
          MethodNotDefined("delete", other.typ).raiseError[M, Expr]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("delete", 1, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 1)
        baseExpr <- evalSingleExpr(p)
        element  <- evalExpr(args.head)
        _        <- charge[M](REMOVE_COST) //TODO(mateusz.gorski): think whether deletion of an element from the collection should dependent on the collection type/size
        result   <- delete(baseExpr, element)
      } yield result
  }

  private[this] val contains: Method = new Method() {

    def contains(baseExpr: Expr, par: Par): M[Expr] =
      baseExpr.exprInstance match {
        case ESetBody(ParSet(basePs, _, _, _)) =>
          (GBool(basePs.contains(par)): Expr).pure[M]
        case EMapBody(ParMap(basePs, _, _, _)) =>
          (GBool(basePs.contains(par)): Expr).pure[M]
        case other => MethodNotDefined("contains", other.typ).raiseError[M, Expr]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("contains", 1, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 1)
        baseExpr <- evalSingleExpr(p)
        element  <- evalExpr(args.head)
        _        <- charge[M](LOOKUP_COST)
        result   <- contains(baseExpr, element)
      } yield result
  }

  private[this] val get: Method = new Method() {

    def get(baseExpr: Expr, key: Par): M[Par] =
      baseExpr.exprInstance match {
        case EMapBody(ParMap(basePs, _, _, _)) =>
          basePs.getOrElse(key, VectorPar()).pure[M]
        case other => MethodNotDefined("get", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("get", 1, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 1)
        baseExpr <- evalSingleExpr(p)
        key      <- evalExpr(args.head)
        _        <- charge[M](LOOKUP_COST)
        result   <- get(baseExpr, key)
      } yield result
  }

  private[this] val getOrElse: Method = new Method {

    def getOrElse(baseExpr: Expr, key: Par, default: Par): M[Par] =
      baseExpr.exprInstance match {
        case EMapBody(ParMap(basePs, _, _, _)) =>
          basePs.getOrElse(key, default).pure[M]
        case other => MethodNotDefined("getOrElse", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("getOrElse", 2, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 2)
        baseExpr <- evalSingleExpr(p)
        key      <- evalExpr(args.head)
        default  <- evalExpr(args(1))
        _        <- charge[M](LOOKUP_COST)
        result   <- getOrElse(baseExpr, key, default)
      } yield result
  }

  private[this] val set: Method = new Method() {

    def set(baseExpr: Expr, key: Par, value: Par): M[Par] =
      baseExpr.exprInstance match {
        case EMapBody(ParMap(basePs, _, _, _)) =>
          (ParMap(basePs + (key -> value)): Par).pure[M]
        case other => MethodNotDefined("set", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("set", 2, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 2)
        baseExpr <- evalSingleExpr(p)
        key      <- evalExpr(args.head)
        value    <- evalExpr(args(1))
        _        <- charge[M](ADD_COST)
        result   <- set(baseExpr, key, value)
      } yield result
  }

  private[this] val keys: Method = new Method() {

    def keys(baseExpr: Expr): M[Par] =
      baseExpr.exprInstance match {
        case EMapBody(ParMap(basePs, _, _, _)) =>
          (ParSet(basePs.keys.toSeq): Par).pure[M]
        case other =>
          MethodNotDefined("keys", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("keys", 0, args.length)
              .raiseError[M, Unit]
              .whenA(args.nonEmpty)
        baseExpr <- evalSingleExpr(p)
        _        <- charge[M](KEYS_METHOD_COST)
        result   <- keys(baseExpr)
      } yield result
  }

  private[this] val size: Method = new Method() {

    def size(baseExpr: Expr): M[(Int, Par)] =
      baseExpr.exprInstance match {
        case EMapBody(ParMap(basePs, _, _, _)) =>
          val size = basePs.size
          (size, GInt(size.toLong): Par).pure[M]
        case ESetBody(ParSet(ps, _, _, _)) =>
          val size = ps.size
          (size, GInt(size.toLong): Par).pure[M]
        case other =>
          MethodNotDefined("size", other.typ).raiseError[M, (Int, Par)]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("size", 0, args.length)
              .raiseError[M, Unit]
              .whenA(args.nonEmpty)
        baseExpr <- evalSingleExpr(p)
        result   <- size(baseExpr)
        _        <- charge[M](sizeMethodCost(result._1))
      } yield result._2
  }

  private[this] val length: Method = new Method() {

    def length(baseExpr: Expr): M[Expr] =
      baseExpr.exprInstance match {
        case GString(string) =>
          (GInt(string.length.toLong): Expr).pure[M]
        case GByteArray(bytes) =>
          (GInt(bytes.size.toLong): Expr).pure[M]
        case EListBody(EList(ps, _, _, _)) =>
          (GInt(ps.length.toLong): Expr).pure[M]
        case other =>
          MethodNotDefined("length", other.typ).raiseError[M, Expr]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("length", 0, args.length)
              .raiseError[M, Unit]
              .whenA(args.nonEmpty)
        baseExpr <- evalSingleExpr(p)
        _        <- charge[M](LENGTH_METHOD_COST)
        result   <- length(baseExpr)
      } yield result
  }

  private[this] val slice: Method = new Method() {

    def slice(baseExpr: Expr, from: Int, until: Int): M[Par] =
      baseExpr.exprInstance match {
        case GString(string) =>
          Sync[M].delay(GString(string.slice(from, until)))
        case EListBody(EList(ps, locallyFree, connectiveUsed, remainder)) =>
          Sync[M].delay(EList(ps.slice(from, until), locallyFree, connectiveUsed, remainder))
        case GByteArray(bytes) =>
          Sync[M].delay(GByteArray(bytes.substring(from, until)))
        case other =>
          MethodNotDefined("slice", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("slice", 2, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 2)
        baseExpr   <- evalSingleExpr(p)
        fromArgRaw <- evalToLong(args.head)
        fromArg    <- restrictToInt(fromArgRaw)
        toArgRaw   <- evalToLong(args(1))
        toArg      <- restrictToInt(toArgRaw)
        _          <- charge[M](sliceCost(toArg))
        result     <- slice(baseExpr, fromArg, toArg)
      } yield result
  }

  private[this] val take: Method = new Method() {
    def take(baseExpr: Expr, n: Int): M[Par] =
      baseExpr.exprInstance match {
        case EListBody(EList(ps, locallyFree, connectiveUsed, remainder)) =>
          Sync[M].delay(EList(ps.take(n), locallyFree, connectiveUsed, remainder))
        case other =>
          MethodNotDefined("take", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("take", 1, args.length)
              .raiseError[M, Unit]
              .whenA(args.length != 1)
        baseExpr <- evalSingleExpr(p)
        nArgRaw  <- evalToLong(args.head)
        nArg     <- restrictToInt(nArgRaw)
        _        <- charge[M](takeCost(nArg))
        result   <- take(baseExpr, nArg)
      } yield result
  }

  private[this] val toList: Method = new Method() {

    def toList(baseExpr: Expr): M[Par] =
      baseExpr.exprInstance match {
        case e: EListBody =>
          (e: Par).pure[M]
        case ESetBody(ParSet(ps, _, _, _)) =>
          charge[M](toListCost(ps.size)) >>
            (EList(ps.toList): Par).pure[M]
        case EMapBody(ParMap(ps, _, _, _)) =>
          charge[M](toListCost(ps.size)) >>
            (EList(
              ps.toSeq.map {
                case (k, v) =>
                  Par().withExprs(Seq(Expr(ETupleBody(ETuple(Seq(k, v))))))
              }
            ): Par).pure[M]
        case ETupleBody(ETuple(ps, _, _)) =>
          charge[M](toListCost(ps.size)) >>
            (EList(ps.toList): Par).pure[M]
        case other =>
          MethodNotDefined("toList", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("toList", 0, args.length)
              .raiseError[M, Unit]
              .whenA(args.nonEmpty)
        baseExpr <- evalSingleExpr(p)
        result   <- toList(baseExpr)
      } yield result
  }

  private[this] val toSet: Method = new Method() {
    def toSet(baseExpr: Expr): M[Par] =
      baseExpr.exprInstance match {
        case e: ESetBody =>
          (e: Par).pure[M]
        case EMapBody(ParMap(basePs, connectiveUsed, locallyFree, remainder)) =>
          (ESetBody(
            ParSet(
              basePs.toSeq.map(t => ETupleBody(ETuple(Seq(t._1, t._2))): Par),
              connectiveUsed,
              locallyFree,
              remainder
            )
          ): Par).pure[M]
        case EListBody(EList(basePs, locallyFree, connectiveUsed, remainder)) =>
          (ESetBody(
            ParSet(
              basePs,
              connectiveUsed,
              locallyFree.get.pure[Coeval],
              remainder
            )
          ): Par).pure[M]
        case other =>
          MethodNotDefined("toSet", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("toSet", 0, args.length)
              .raiseError[M, Unit]
              .whenA(args.nonEmpty)
        baseExpr <- evalSingleExpr(p)
        result   <- toSet(baseExpr)
      } yield result
  }

  private[this] val toMap: Method = new Method() {
    def makeMap(
        ps: Seq[Par],
        connectiveUsed: Boolean,
        locallyFree: Coeval[BitSet],
        remainder: Option[Var]
    ) = {
      val keyPairs = ps.map(RhoType.Tuple2.unapply)
      if (keyPairs.exists(_.isEmpty))
        MethodNotDefined("toMap", "types except List[(K,V)]").raiseError[M, Par]
      else
        (EMapBody(
          ParMap(
            keyPairs.flatMap(_.toList),
            connectiveUsed,
            locallyFree,
            remainder
          )
        ): Par).pure[M]
    }
    def toMap(baseExpr: Expr): M[Par] =
      baseExpr.exprInstance match {
        case e: EMapBody =>
          (e: Par).pure[M]
        case ESetBody(ParSet(basePs, connectiveUsed, locallyFree, remainder)) =>
          makeMap(basePs.toSeq, connectiveUsed, locallyFree, remainder)
        case EListBody(EList(basePs, locallyFree, connectiveUsed, remainder)) =>
          makeMap(basePs, connectiveUsed, locallyFree.get.pure[Coeval], remainder)
        case other =>
          MethodNotDefined("toMap", other.typ).raiseError[M, Par]
      }

    override def apply(p: Par, args: Seq[Par])(implicit env: Env[Par]): M[Par] =
      for {
        _ <- MethodArgumentNumberMismatch("toMap", 0, args.length)
              .raiseError[M, Unit]
              .whenA(args.nonEmpty)
        baseExpr <- evalSingleExpr(p)
        result   <- toMap(baseExpr)
      } yield result
  }

  private val methodTable: Map[String, Method] =
    Map(
      "nth"         -> nth,
      "toByteArray" -> toByteArray,
      "hexToBytes"  -> hexToBytes,
      "bytesToHex"  -> bytesToHex,
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
      "slice"       -> slice,
      "take"        -> take,
      "toList"      -> toList,
      "toSet"       -> toSet,
      "toMap"       -> toMap
    )

  private def evalSingleExpr(p: Par)(implicit env: Env[Par]): M[Expr] =
    if (p.sends.nonEmpty || p.receives.nonEmpty || p.news.nonEmpty || p.matches.nonEmpty || p.unforgeables.nonEmpty || p.bundles.nonEmpty)
      ReduceError("Error: parallel or non expression found where expression expected.")
        .raiseError[M, Expr]
    else
      p.exprs match {
        case (e: Expr) +: Nil => evalExprToExpr(e)
        case _                => ReduceError("Error: Multiple expressions given.").raiseError[M, Expr]
      }

  private def evalToLong(
      p: Par
  )(implicit env: Env[Par]): M[Long] =
    if (p.sends.nonEmpty || p.receives.nonEmpty || p.news.nonEmpty || p.matches.nonEmpty || p.unforgeables.nonEmpty || p.bundles.nonEmpty)
      ReduceError("Error: parallel or non expression found where expression expected.")
        .raiseError[M, Long]
    else
      p.exprs match {
        case Expr(GInt(v)) +: Nil =>
          (v: Long).pure[M]
        case Expr(EVarBody(EVar(v))) +: Nil =>
          for {
            p      <- eval(v)
            intVal <- evalToLong(p)
          } yield intVal
        case (e: Expr) +: Nil =>
          for {
            evaled <- evalExprToExpr(e)
            result <- evaled.exprInstance match {
                       case GInt(v) =>
                         (v: Long).pure[M]
                       case _ =>
                         ReduceError("Error: expression didn't evaluate to integer.")
                           .raiseError[M, Long]
                     }
          } yield result
        case _ =>
          ReduceError("Error: Integer expected, or unimplemented expression.").raiseError[M, Long]
      }

  private def evalToBool(
      p: Par
  )(implicit env: Env[Par]): M[Boolean] =
    if (p.sends.nonEmpty || p.receives.nonEmpty || p.news.nonEmpty || p.matches.nonEmpty || p.unforgeables.nonEmpty || p.bundles.nonEmpty)
      ReduceError("Error: parallel or non expression found where expression expected.")
        .raiseError[M, Boolean]
    else
      p.exprs match {
        case Expr(GBool(b)) +: Nil =>
          (b: Boolean).pure[M]
        case Expr(EVarBody(EVar(v))) +: Nil =>
          for {
            p       <- eval(v)
            boolVal <- evalToBool(p)
          } yield boolVal
        case (e: Expr) +: Nil =>
          for {
            evaled <- evalExprToExpr(e)
            result <- evaled.exprInstance match {
                       case GBool(b) => (b: Boolean).pure[M]
                       case _ =>
                         ReduceError("Error: expression didn't evaluate to boolean.")
                           .raiseError[M, Boolean]
                     }
          } yield result
        case _ => ReduceError("Error: Multiple expressions given.").raiseError[M, Boolean]
      }

  private def restrictToInt(long: Long): M[Int] =
    Sync[M].catchNonFatal(Math.toIntExact(long)).adaptError {
      case _: ArithmeticException => ReduceError(s"Integer overflow for value $long")
    }

  private def updateLocallyFree(par: Par): Par =
    par.copy(
      locallyFree = par.sends.foldLeft(BitSet())((acc, send) => acc | send.locallyFree) |
        par.receives.foldLeft(BitSet())((acc, receive) => acc | receive.locallyFree) |
        par.news.foldLeft(BitSet())((acc, newProc) => acc | newProc.locallyFree) |
        par.exprs.foldLeft(BitSet())((acc, expr) => acc | ExprLocallyFree.locallyFree(expr, 0)) |
        par.matches.foldLeft(BitSet())((acc, matchProc) => acc | matchProc.locallyFree) |
        par.bundles.foldLeft(BitSet())((acc, bundleProc) => acc | bundleProc.locallyFree)
    )

  private def updateLocallyFree(elist: EList): EList =
    elist.copy(locallyFree = elist.ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree))

  private def updateLocallyFree(elist: ETuple): ETuple =
    elist.copy(locallyFree = elist.ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree))

  /**
    * Evaluate any top level expressions in @param Par .
    */
  private[rholang] def evalExpr(par: Par)(implicit env: Env[Par]): M[Par] =
    for {
      evaledExprs <- par.exprs.toList.traverse(evalExprToPar)
      // Note: the locallyFree cache in par could now be invalid, but given
      // that locallyFree is for use in the matcher, and the matcher uses
      // substitution, it will resolve in that case. AlwaysEqual makes sure
      // that this isn't an issue in the rest of cases.
      result = evaledExprs.foldLeft(par.copy(exprs = Vector()))(_ ++ _)
    } yield result
}
