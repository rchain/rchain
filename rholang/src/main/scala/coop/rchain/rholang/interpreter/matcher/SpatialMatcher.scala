package coop.rchain.rholang.interpreter.matcher

import cats.effect.Sync
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._
import cats.{FlatMap, Monad, MonoidK, Eval => _}
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{FreeVar, Wildcard}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits.{VectorPar, _}
import coop.rchain.rholang.interpreter._
import coop.rchain.rholang.interpreter.Splittable
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.{BugFoundError, OutOfPhlogistonsError}
import coop.rchain.rholang.interpreter.matcher.ParSpatialMatcherUtils.{noFrees, subPars}
import coop.rchain.rholang.interpreter.matcher.SpatialMatcher._
import coop.rchain.rholang.interpreter.matcher.StreamT._

import scala.collection.mutable

// The spatial matcher takes targets and patterns. It uses StateT[Option,
// FreeMap, Unit] to represent the computation. The state is the mapping from
// free variables to the values that were captured. StateT[Option, S, A] allows
// for failure, and when a failure occurs, no new state is provided. This is
// what we want, because when no matching occurs, there is no result map.
// In a few places we use StateT[Stream, FreeMap, A] for backtracking. In order
// to help cut down on backtracking, wherever one of several possible matches
// will do, we just take one.
trait SpatialMatcher[F[_], T, P] {
  def spatialMatch(target: T, pattern: P): F[Unit]
}

object SpatialMatcher extends SpatialMatcherInstances {

  //TODO make the wrapper typeclasses more usable by removing the terminating `_`.
  //Otherwise e.g. `Alternative_: Monad:` fails to compile b/c `:` is treated as part of the `Alternative_` identifier
  //Also, the following workaround is needed b/c of precisely this reason:
  type Alternative[F[_]] = Alternative_[F]

  def spatialMatchAndCharge[M[_]: Sync](target: Par, pattern: Par)(
      implicit
      cost: _cost[M]
  ): M[Option[(FreeMap, Unit)]] = {
    type R[A] = MatcherMonadT[M, A]

    val doMatch: R[Unit] = SpatialMatcher.spatialMatch[R, Par, Par](target, pattern)

    val matchAndCharge: M[Option[(FreeMap, Unit)]] = for {
      phlosAvailable <- cost.get
      result <- runFirstWithCost[M, Unit](doMatch, phlosAvailable).onError {
                 case OutOfPhlogistonsError =>
                   // if we run out of phlos during the match we have to zero phlos available
                   cost.get.flatMap(ca => charge[M](ca))
               }
      (phlosLeft, matchResult) = result
      matchCost                = phlosAvailable - phlosLeft
      _                        <- charge[M](matchCost)
    } yield matchResult

    matchAndCharge
  }

  def spatialMatch[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short, T, P](
      target: T,
      pattern: P
  )(
      implicit sm: SpatialMatcher[F, T, P]
  ): F[Unit] =
    SpatialMatcher[F, T, P].spatialMatch(target, pattern)

  def apply[F[_], T, P](implicit sm: SpatialMatcher[F, T, P]) = sm

  implicit def forTuple[F[_]: FlatMap, A, B, C, D](
      implicit matcherAC: SpatialMatcher[F, A, C],
      matcherBD: SpatialMatcher[F, B, D]
  ): SpatialMatcher[F, (A, B), (C, D)] = new SpatialMatcher[F, (A, B), (C, D)] {
    override def spatialMatch(target: (A, B), pattern: (C, D)): F[Unit] =
      matcherAC.spatialMatch(target._1, pattern._1) >> matcherBD.spatialMatch(target._2, pattern._2)
  }

  def fromFunction[F[_], T, P](fn: (T, P) => F[Unit]): SpatialMatcher[F, T, P] =
    new SpatialMatcher[F, T, P] {
      override def spatialMatch(target: T, pattern: P): F[Unit] =
        fn(target, pattern)
    }

  // This helper function is useful in several productions
  def foldMatch[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short, T, P](
      tlist: Seq[T],
      plist: Seq[P],
      remainder: Option[Var] = None
  )(
      implicit lft: HasLocallyFree[T],
      sm: SpatialMatcher[F, T, P]
  ): F[Seq[T]] =
    (tlist, plist) match {
      case (Nil, Nil) => Seq.empty[T].pure[F]
      case (Nil, _) =>
        MonoidK[F].empty[Seq[T]]
      case (trem, Nil) =>
        remainder match {
          case None =>
            MonoidK[F].empty[Seq[T]]
          case Some(Var(FreeVar(level))) => {
            def freeCheck(trem: Seq[T], level: Int, acc: Seq[T]): F[Seq[T]] =
              trem match {
                case Nil => acc.pure[F]
                case item +: rem =>
                  if (lft.locallyFree(item, 0).isEmpty)
                    freeCheck(rem, level, acc :+ item)
                  else
                    MonoidK[F].empty[Seq[T]]
              }
            freeCheck(trem, level, Vector.empty[T])
          }
          case Some(Var(Wildcard(_))) => Seq.empty[T].pure[F]
          case _                      => MonoidK[F].empty[Seq[T]]
        }
      case (t +: trem, p +: prem) =>
        spatialMatch(t, p).flatMap(_ => foldMatch(trem, prem, remainder))
    }

  def listMatchSingle[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short, T](
      tlist: Seq[T],
      plist: Seq[T]
  )(implicit lf: HasLocallyFree[T], sm: SpatialMatcher[F, T, T]): F[Unit] =
    listMatchSingle_(tlist, plist, (p: Par, _: Seq[T]) => p, None, false)

  /** This function finds a single matching from a list of patterns and a list of targets.
    * Any remaining terms are either grouped with the free variable varLevel or thrown away with the wildcard.
    * If both are provided, we prefer to capture terms that can be captured.
    *
    * The '_' in the name is to avoid overloading and thus having to provide parameter types for `merger` explicitly...
    *
    * @param tlist  the target list
    * @param plist  the pattern list
    * @param merger a function that sets a Par's field to the Seq of captured T-s. Used for updating the state map.
    * @param remainder if non-empty, the free variable level where to put the remaining T's
    * @param wildcard if true, there is a wildcard in parallel with the pattern list.
    * @param lf
    * @param sm a function that does a spatial match between T's
    * @tparam T
    * @return
    */
  def listMatchSingle_[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short, T](
      tlist: Seq[T],
      plist: Seq[T],
      merger: (Par, Seq[T]) => Par,
      remainder: Option[Int],
      wildcard: Boolean
  )(implicit lf: HasLocallyFree[T], sm: SpatialMatcher[F, T, T]): F[Unit] = {
    val exactMatch = !wildcard && remainder.isEmpty
    val plen       = plist.length
    val tlen       = tlist.length

    val result: F[Unit] =
      if (exactMatch && plen != tlen)
        charge[F](COMPARISON_COST) *> MonoidK[F].empty[Unit]
      else if (plen > tlen)
        charge[F](COMPARISON_COST) *> MonoidK[F].empty[Unit]
      else if (plen == 0 && tlen == 0 && remainder.isEmpty)
        ().pure[F]
      else if (plen == 0 && remainder.isDefined) {
        val matchResult =
          if (tlist.forall(lf.locallyFree(_, 0).isEmpty))
            handleRemainder[F, T](tlist, remainder.get, merger)
          else
            MonoidK[F].empty[Unit]
        charge[F](COMPARISON_COST * tlist.size) *> matchResult
      } else
        listMatch(tlist, plist, merger, remainder, wildcard)

    result
  }

  def listMatch[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short, T](
      targets: Seq[T],
      patterns: Seq[T],
      merger: (Par, Seq[T]) => Par,
      remainder: Option[Int],
      wildcard: Boolean
  )(implicit lf: HasLocallyFree[T], sm: SpatialMatcher[F, T, T]): F[Unit] = {

    sealed trait Pattern
    final case class Term(term: T)         extends Pattern
    final case class Remainder(level: Int) extends Pattern

    val remainderPatterns: Seq[Pattern] = remainder.fold(
      Seq.empty[Pattern]
    )(
      level => Seq.fill(targets.size - patterns.size)(Remainder(level))
    )
    val allPatterns = remainderPatterns ++ patterns.map(Term)

    val matchFunction: (Pattern, T) => F[Option[FreeMap]] =
      (pattern: Pattern, t: T) => {
        val matchEffect = pattern match {
          case Term(p) =>
            if (!lf.connectiveUsed(p)) {
              //match using `==` if pattern is a concrete term
              charge[F](COMPARISON_COST) *> Alternative_[F].guard(t == p)
            } else {
              spatialMatch(t, p)
            }
          case Remainder(_) =>
            //Remainders can't match non-concrete terms, because they can't be captured.
            //They match everything that's concrete though.
            charge[F](COMPARISON_COST) *> Alternative_[F].guard(lf.locallyFree(t, 0).isEmpty)
        }
        attemptOpt[F, FreeMap](isolateState[F, FreeMap](matchEffect))
      }
    val maximumBipartiteMatch = MaximumBipartiteMatch(memoizeInHashMap(matchFunction))

    for {
      matches                <- Alternative_[F].unite(maximumBipartiteMatch.findMatches(allPatterns, targets))
      freeMaps               = matches.map(_._3)
      updatedFreeMap         <- aggregateUpdates(freeMaps)
      _                      <- _freeMap[F].set(updatedFreeMap)
      remainderTargets       = matches.collect { case (target, _: Remainder, _) => target }
      remainderTargetsSet    = remainderTargets.toSet
      remainderTargetsSorted = targets.filter(remainderTargetsSet.contains)
      _ <- remainder match {
            case None =>
              // If there is a wildcard, we succeed.
              // We also succeed if there isn't but the remainder is empty.
              if (wildcard || remainderTargetsSorted.isEmpty)
                ().pure[F]
              else
                // This should be prevented by the length checks.
                MonoidK[F].empty
            // If there's a capture variable, we prefer to add things to that rather than throw them away.
            case Some(level) => {
              handleRemainder[F, T](remainderTargetsSorted, level, merger)
            }
          }
    } yield ()
  }

  private def isolateState[H[_]: MonadState[?[_], S], S](f: H[_]): H[S] = {
    implicit val M = MonadState[H, S].monad
    for {
      initState   <- MonadState[H, S].get
      _           <- f
      resultState <- MonadState[H, S].get
      _           <- MonadState[H, S].set(initState)
    } yield resultState
  }

  private def memoizeInHashMap[A, B, C](f: (A, B) => C): (A, B) => C = {
    val memo = mutable.HashMap[(A, B), C]()
    (a, b) => memo.getOrElseUpdate((a, b), f(a, b))
  }

  private def aggregateUpdates[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap](
      freeMaps: Seq[FreeMap]
  ): F[FreeMap] =
    for {
      currentFreeMap <- _freeMap[F].get
      currentVars    = currentFreeMap.keys.toSet
      addedVars      = freeMaps.flatMap(_.keys.filterNot(currentVars.contains))
      _ <- _error[F].ensure(addedVars.pure[F])(
            BugFoundError(s"Aggregated updates conflicted with each other: $freeMaps")
          ) {
            //The correctness of isolating MBM from changing FreeMap relies
            //on our ability to aggregate the var assignments from subsequent matches.
            //This means all the variables populated by MBM must not duplicate each other.
            addedVars =>
              addedVars.size == addedVars.distinct.size
          }
      updatedFreeMap = freeMaps.fold(currentFreeMap)(_ ++ _)
    } yield updatedFreeMap

  private def handleRemainder[G[_]: Monad, T](
      remainderTargets: Seq[T],
      level: Int,
      merger: (Par, Seq[T]) => Par
  )(
      implicit freeMap: _freeMap[G]
  ): G[Unit] =
    for {
      remainderPar <- freeMap.inspect[Par](_.getOrElse(level, VectorPar()))
      //TODO: enforce sorted-ness of returned terms using types / by verifying the sorted-ness here
      remainderParUpdated = merger(remainderPar, remainderTargets)
      _                   <- freeMap.modify(_ + (level -> remainderParUpdated))
    } yield ()

}

trait SpatialMatcherInstances {
  // This matches a single logical connective against a Par. If it can match
  // more ways than one, we don't care, as we care about what went into the
  // match, and not about how it matched inside. The tricky part goes into
  // the par/par matcher.
  import Splittable.SplittableOps

  implicit def connectiveMatcher[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, Par, Connective] =
    fromFunction[F, Par, Connective] { (target, pattern) =>
      pattern.connectiveInstance match {
        case ConnAndBody(ConnectiveBody(ps)) =>
          ps.toList.traverse_(p => spatialMatch(target, p))
        case ConnOrBody(ConnectiveBody(ps)) =>
          val freeMap = _freeMap[F]
          val allMatches = for {
            p       <- Alternative_[F].unite(ps.toList.pure[F])
            matches <- freeMap.get
            _       <- spatialMatch(target, p)
            _       <- freeMap.set(matches)
          } yield ()
          allMatches.takeFirst()

        case ConnNotBody(p) =>
          attemptOpt[F, Unit](spatialMatch(target, p)).flatMap {
            case None    => ().pure[F]
            case Some(_) => MonoidK[F].empty
          }

        case _: VarRefBody =>
          // this should never happen because variable references should be substituted
          MonoidK[F].empty[Unit]

        case _: ConnBool =>
          target.singleExpr match {
            case Some(Expr(GBool(_))) =>
              ().pure[F]
            case _ => MonoidK[F].empty[Unit]
          }

        case _: ConnInt =>
          target.singleExpr match {
            case Some(Expr(GInt(_))) =>
              ().pure[F]
            case _ => MonoidK[F].empty[Unit]
          }

        case _: ConnString =>
          target.singleExpr match {
            case Some(Expr(GString(_))) =>
              ().pure[F]
            case _ => MonoidK[F].empty[Unit]
          }

        case _: ConnUri =>
          target.singleExpr match {
            case Some(Expr(GUri(_))) =>
              ().pure[F]
            case _ => MonoidK[F].empty[Unit]
          }

        case _: ConnByteArray =>
          target.singleExpr match {
            case Some(Expr(GByteArray(_))) =>
              ().pure[F]
            case _ => MonoidK[F].empty[Unit]
          }

        case ConnectiveInstance.Empty =>
          MonoidK[F].empty[Unit]
      }
    }

  implicit def parSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, Par, Par] = fromFunction[F, Par, Par] { (target, pattern) =>
    if (!pattern.connectiveUsed) {
      val cost = equalityCheckCost(pattern, target)
      if (pattern == target)
        charge[F](cost) *> ().pure[F]
      else {
        charge[F](cost) *> MonoidK[F].empty[Unit]
      }
    } else {

      val varLevel: Option[Int] = pattern.exprs.collectFirst[Int] {
        case Expr(EVarBody(EVar(Var(FreeVar(level))))) => level
      }

      val wildcard: Boolean = pattern.exprs.collectFirst {
        case Expr(EVarBody(EVar(Var(Wildcard(_))))) => ()
      }.isDefined

      val filteredPattern  = noFrees(pattern)
      val pc               = ParCount(filteredPattern)
      val minRem           = pc
      val maxRem           = if (wildcard || !varLevel.isEmpty) ParCount.max else pc
      val individualBounds = filteredPattern.connectives.map(ParCount.minMax)
      val remainderBounds = individualBounds
        .scanRight((minRem, maxRem)) { (bounds, acc) =>
          val result = (bounds._1 + acc._1, bounds._2 + acc._2)
          result
        }
        .tail
      val connectivesWithBounds =
        (filteredPattern.connectives, individualBounds, remainderBounds).zipped.toList

      def matchConnectiveWithBounds(
          target: Par,
          labeledConnective: (Connective, (ParCount, ParCount), (ParCount, ParCount))
      ): F[Par] = {
        val (con, bounds, remainders) = labeledConnective
        for {
          sp <- Alternative_[F].unite(
                 subPars(target, bounds._1, bounds._2, remainders._1, remainders._2).pure[F]
               )
          _ <- spatialMatch(sp._1, con)
        } yield sp._2
      }
      for {
        remainder <- connectivesWithBounds.foldM(target)(matchConnectiveWithBounds)
        _ <- listMatchSingle_[F, Send](
              remainder.sends,
              pattern.sends,
              (p, s) => p.withSends(s),
              varLevel,
              wildcard
            )
        _ <- listMatchSingle_[F, Receive](
              remainder.receives,
              pattern.receives,
              (p, s) => p.withReceives(s),
              varLevel,
              wildcard
            )
        _ <- listMatchSingle_[F, New](
              remainder.news,
              pattern.news,
              (p, s) => p.withNews(s),
              varLevel,
              wildcard
            )
        _ <- listMatchSingle_[F, Expr](
              remainder.exprs,
              noFrees(pattern.exprs),
              (p, e) => p.withExprs(e),
              varLevel,
              wildcard
            )
        _ <- listMatchSingle_[F, Match](
              remainder.matches,
              pattern.matches,
              (p, e) => p.withMatches(e),
              varLevel,
              wildcard
            )
        _ <- listMatchSingle_[F, Bundle](
              remainder.bundles,
              pattern.bundles,
              (p, b) => p.withBundles(b),
              varLevel,
              wildcard
            )
        _ <- listMatchSingle_[F, GPrivate](
              remainder.ids,
              pattern.ids,
              (p, i) => p.withIds(i),
              varLevel,
              wildcard
            )
      } yield ()
    }
  }

  implicit def bundleSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost]
    : SpatialMatcher[F, Bundle, Bundle] =
    fromFunction[F, Bundle, Bundle] { (target, pattern) =>
      val cost = equalityCheckCost(target, pattern)
      if (pattern == target)
        charge[F](cost) *> ().pure[F]
      else {
        charge[F](cost) *> MonoidK[F].empty[Unit]
      }
    }

  implicit def sendSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, Send, Send] = fromFunction[F, Send, Send] { (target, pattern) =>
    if (target.persistent != pattern.persistent)
      charge[F](COMPARISON_COST) *> MonoidK[F].empty[Unit]
    else
      for {
        _ <- spatialMatch(target.chan, pattern.chan)
        _ <- foldMatch(target.data, pattern.data)
      } yield ()
  }

  implicit def receiveSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, Receive, Receive] =
    fromFunction[F, Receive, Receive] { (target, pattern) =>
      if (target.persistent != pattern.persistent)
        charge[F](COMPARISON_COST) *> MonoidK[F].empty[Unit]
      else
        for {
          _ <- listMatchSingle(target.binds, pattern.binds)
          _ <- spatialMatch(target.body, pattern.body)
        } yield ()
    }

  implicit def newSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, New, New] = fromFunction[F, New, New] { (target, pattern) =>
    if (target.bindCount == pattern.bindCount)
      charge[F](COMPARISON_COST) *> spatialMatch(target.p, pattern.p)
    else
      charge[F](COMPARISON_COST) *> MonoidK[F].empty[Unit]
  }

  implicit def exprSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, Expr, Expr] = fromFunction[F, Expr, Expr] { (target, pattern) =>
    (target.exprInstance, pattern.exprInstance) match {
      case (EListBody(EList(tlist, _, _, _)), EListBody(EList(plist, _, _, rem))) => {
        for {
          matchedRem <- foldMatch(tlist, plist, rem)
          _ <- rem match {
                case Some(Var(FreeVar(level))) =>
                  _freeMap[F].modify(m => m + (level -> EList(matchedRem)))
                case _ => ().pure[F]
              }
        } yield ()
      }
      case (ETupleBody(ETuple(tlist, _, _)), ETupleBody(ETuple(plist, _, _))) => {
        foldMatch(tlist, plist).map(_ => ())
      }
      case (ESetBody(ParSet(tlist, _, _, _)), ESetBody(ParSet(plist, _, _, rem))) =>
        val isWildcard      = rem.collect { case Var(Wildcard(_)) => true }.isDefined
        val remainderVarOpt = rem.collect { case Var(FreeVar(level)) => level }
        val merger          = (p: Par, r: Seq[Par]) => p.withExprs(Seq(ParSet(r)))
        listMatchSingle_(tlist.toSeq, plist.toSeq, merger, remainderVarOpt, isWildcard)

      case (EMapBody(ParMap(tlist, _, _, _)), EMapBody(ParMap(plist, _, _, rem))) =>
        val isWildcard      = rem.collect { case Var(Wildcard(_)) => true }.isDefined
        val remainderVarOpt = rem.collect { case Var(FreeVar(level)) => level }
        val merger          = (p: Par, r: Seq[(Par, Par)]) => p.withExprs(Seq(ParMap(r)))
        listMatchSingle_(tlist.toSeq, plist.toSeq, merger, remainderVarOpt, isWildcard)

      case (EVarBody(EVar(vp)), EVarBody(EVar(vt))) =>
        val cost = equalityCheckCost(vp, vt)
        if (vp == vt)
          charge[F](cost) *> ().pure[F]
        else
          charge[F](cost) *> MonoidK[F].empty[Unit]
      case (ENotBody(ENot(t)), ENotBody(ENot(p))) => spatialMatch(t, p)
      case (ENegBody(ENeg(t)), ENegBody(ENeg(p))) => spatialMatch(t, p)
      case (EMultBody(EMult(t1, t2)), EMultBody(EMult(p1, p2))) =>
        for {
          _ <- spatialMatch(t1, p1)
          _ <- spatialMatch(t2, p2)
        } yield ()
      case (EDivBody(EDiv(t1, t2)), EDivBody(EDiv(p1, p2))) =>
        for {
          _ <- spatialMatch(t1, p1)
          _ <- spatialMatch(t2, p2)
        } yield ()
      case (
          EPercentPercentBody(EPercentPercent(t1, t2)),
          EPercentPercentBody(EPercentPercent(p1, p2))
          ) =>
        for {
          _ <- spatialMatch(t1, p1)
          _ <- spatialMatch(t2, p2)
        } yield ()
      case (EPlusBody(EPlus(t1, t2)), EPlusBody(EPlus(p1, p2))) =>
        for {
          _ <- spatialMatch(t1, p1)
          _ <- spatialMatch(t2, p2)
        } yield ()
      case (EPlusPlusBody(EPlusPlus(t1, t2)), EPlusPlusBody(EPlusPlus(p1, p2))) =>
        for {
          _ <- spatialMatch(t1, p1)
          _ <- spatialMatch(t2, p2)
        } yield ()
      case (EMinusMinusBody(EMinusMinus(t1, t2)), EMinusMinusBody(EMinusMinus(p1, p2))) =>
        for {
          _ <- spatialMatch(t1, p1)
          _ <- spatialMatch(t2, p2)
        } yield ()
      case _ => MonoidK[F].empty[Unit]
    }
  }

  implicit def matchSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, Match, Match] =
    fromFunction[F, Match, Match] { (target, pattern) =>
      for {
        _ <- spatialMatch(target.target, pattern.target)
        _ <- foldMatch(target.cases, pattern.cases)
      } yield ()
    }

  /**
    * Note that currently there should be no way to put a GPrivate in a pattern
    * because patterns start with an empty environment.
    * We're going to write the obvious definition anyway.
    */
  implicit def gprivateSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap]
    : SpatialMatcher[F, GPrivate, GPrivate] =
    fromFunction[F, GPrivate, GPrivate] { (target, pattern) =>
      if (target == pattern) {
        val cost = equalityCheckCost(target, pattern)
        charge[F](cost) *> ().pure[F]
      } else
        MonoidK[F].empty
    }

  implicit def receiveBindSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, ReceiveBind, ReceiveBind] =
    fromFunction[F, ReceiveBind, ReceiveBind] { (target, pattern) =>
      if (target.patterns != pattern.patterns) {
        val cost: Cost =
          target.patterns
            .zip(pattern.patterns)
            .map(x => equalityCheckCost(x._1, x._2))
            .foldLeft(Cost(0))(_ + _)
        charge[F](cost) *> MonoidK[F].empty
      } else
        spatialMatch(target.source, pattern.source)
    }

  implicit def matchCaseSpatialMatcherInstance[F[_]: Splittable: Alternative: Monad: _error: _cost: _freeMap: _short]
    : SpatialMatcher[F, MatchCase, MatchCase] =
    fromFunction[F, MatchCase, MatchCase] { (target, pattern) =>
      if (target.pattern != pattern.pattern) {
        val cost: Cost = equalityCheckCost(target.pattern, pattern.pattern)
        charge[F](cost) *> MonoidK[F].empty
      } else
        spatialMatch(target.source, pattern.source)
    }

}
