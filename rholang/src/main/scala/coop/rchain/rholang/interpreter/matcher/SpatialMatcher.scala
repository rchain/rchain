package coop.rchain.rholang.interpreter.matcher

import cats.data.{OptionT, StateT}
import cats.effect.Sync
import cats.implicits._
import cats.{Monad, Eval => _}
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits.{VectorPar, _}
import coop.rchain.rholang.interpreter.accounting.{Cost, _}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.matcher.NonDetFreeMapWithCost._
import coop.rchain.rholang.interpreter.matcher.OptionalFreeMapWithCost._
import coop.rchain.rholang.interpreter.matcher.SpatialMatcher._
import coop.rchain.rholang.interpreter.matcher.StreamT._

import scala.collection.immutable.Stream
import scala.collection.mutable

// The spatial matcher takes targets and patterns. It uses StateT[Option,
// FreeMap, Unit] to represent the computation. The state is the mapping from
// free variables to the values that were captured. StateT[Option, S, A] allows
// for failure, and when a failure occurs, no new state is provided. This is
// what we want, because when no matching occurs, there is no result map.
// In a few places we use StateT[Stream, FreeMap, A] for backtracking. In order
// to help cut down on backtracking, wherever one of several possible matches
// will do, we just take one.
trait SpatialMatcher[T, P] {
  def spatialMatch(target: T, pattern: P): OptionalFreeMapWithCost[Unit]
  def nonDetMatch(target: T, pattern: P): NonDetFreeMapWithCost[Unit]
}

object SpatialMatcher extends SpatialMatcherInstances {

  def spatialMatchAndCharge[M[_]: Sync](target: Par, pattern: Par)(
      implicit costAlg: CostAccounting[M]
  ): M[Option[(FreeMap, Unit)]] =
    for {
      // phlos available before going to the matcher
      phlosAvailable <- costAlg.get()
      result <- Sync[M]
                 .fromEither(
                   SpatialMatcher
                     .spatialMatch(target, pattern)
                     .runWithCost(phlosAvailable.cost)
                 )
                 .flatMap {
                   case (phlosLeft, result) =>
                     val matchCost = phlosAvailable.cost - phlosLeft
                     costAlg.charge(matchCost).map(_ => result)
                 }
                 .onError {
                   case OutOfPhlogistonsError =>
                     // if we run out of phlos during the match we have to zero phlos available
                     costAlg.get().flatMap(ca => costAlg.charge(ca.cost))
                 }
    } yield result

  def spatialMatch[T, P](target: T, pattern: P)(
      implicit sm: SpatialMatcher[T, P]
  ): OptionalFreeMapWithCost[Unit] =
    SpatialMatcher[T, P].spatialMatch(target, pattern)

  def nonDetMatch[T, P](target: T, pattern: P)(
      implicit sm: SpatialMatcher[T, P]
  ): NonDetFreeMapWithCost[Unit] =
    SpatialMatcher[T, P].nonDetMatch(target, pattern)

  def apply[T, P](implicit sm: SpatialMatcher[T, P]) = sm

  implicit def forTuple[A, B, C, D](
      implicit matcherAC: SpatialMatcher[A, C],
      matcherBD: SpatialMatcher[B, D]
  ): SpatialMatcher[(A, B), (C, D)] = new SpatialMatcher[(A, B), (C, D)] {
    override def spatialMatch(target: (A, B), pattern: (C, D)): OptionalFreeMapWithCost[Unit] =
      matcherAC.spatialMatch(target._1, pattern._1) >> matcherBD.spatialMatch(target._2, pattern._2)

    override def nonDetMatch(target: (A, B), pattern: (C, D)): NonDetFreeMapWithCost[Unit] =
      matcherAC.nonDetMatch(target._1, pattern._1) >> matcherBD.nonDetMatch(target._2, pattern._2)
  }

  def fromFunction[T, P](fn: (T, P) => OptionalFreeMapWithCost[Unit]): SpatialMatcher[T, P] =
    new SpatialMatcher[T, P] {
      override def spatialMatch(target: T, pattern: P): OptionalFreeMapWithCost[Unit] =
        fn(target, pattern)
      override def nonDetMatch(target: T, pattern: P): NonDetFreeMapWithCost[Unit] =
        fn(target, pattern).toNonDet()
    }

  def fromNonDetFunction[T, P](fn: (T, P) => NonDetFreeMapWithCost[Unit]): SpatialMatcher[T, P] =
    new SpatialMatcher[T, P] {
      override def nonDetMatch(target: T, pattern: P): NonDetFreeMapWithCost[Unit] =
        fn(target, pattern)
      override def spatialMatch(target: T, pattern: P): OptionalFreeMapWithCost[Unit] =
        fn(target, pattern).toDet()
    }

  def noFrees(par: Par): Par =
    par.withExprs(noFrees(par.exprs))

  def noFrees(exprs: Seq[Expr]): Seq[Expr] =
    exprs.filter({ (expr) =>
      expr.exprInstance match {
        case EVarBody(EVar(v)) =>
          v.varInstance match {
            case FreeVar(_)  => false
            case Wildcard(_) => false
            case _           => true
          }
        case _ => true
      }
    })

  def subPars(
      par: Par,
      min: ParCount,
      max: ParCount,
      minPrune: ParCount,
      maxPrune: ParCount
  ): Stream[(Par, Par)] = {

    val sendMax    = math.min(max.sends, par.sends.size - minPrune.sends)
    val receiveMax = math.min(max.receives, par.receives.size - minPrune.receives)
    val newMax     = math.min(max.news, par.news.size - minPrune.news)
    val exprMax    = math.min(max.exprs, par.exprs.size - minPrune.exprs)
    val matchMax   = math.min(max.matches, par.matches.size - minPrune.matches)
    val idMax      = math.min(max.ids, par.ids.size - minPrune.ids)
    val bundleMax  = math.min(max.bundles, par.bundles.size - minPrune.bundles)

    val sendMin    = math.max(min.sends, par.sends.size - maxPrune.sends)
    val receiveMin = math.max(min.receives, par.receives.size - maxPrune.receives)
    val newMin     = math.max(min.news, par.news.size - maxPrune.news)
    val exprMin    = math.max(min.exprs, par.exprs.size - maxPrune.exprs)
    val matchMin   = math.max(min.matches, par.matches.size - maxPrune.matches)
    val idMin      = math.max(min.ids, par.ids.size - maxPrune.ids)
    val bundleMin  = math.max(min.bundles, par.bundles.size - maxPrune.bundles)

    for {
      subSends    <- minMaxSubsets(par.sends, sendMin, sendMax)
      subReceives <- minMaxSubsets(par.receives, receiveMin, receiveMax)
      subNews     <- minMaxSubsets(par.news, newMin, newMax)
      subExprs    <- minMaxSubsets(par.exprs, exprMin, exprMax)
      subMatches  <- minMaxSubsets(par.matches, matchMin, matchMax)
      subIds      <- minMaxSubsets(par.ids, idMin, idMax)
      subBundles  <- minMaxSubsets(par.bundles, bundleMin, bundleMax)
    } yield
      (
        Par(
          subSends._1,
          subReceives._1,
          subNews._1,
          subExprs._1,
          subMatches._1,
          subIds._1,
          subBundles._1
        ),
        Par(
          subSends._2,
          subReceives._2,
          subNews._2,
          subExprs._2,
          subMatches._2,
          subIds._2,
          subBundles._2
        )
      )
  }

  def minMaxSubsets[A](as: Seq[A], minSize: Int, maxSize: Int): Stream[(Seq[A], Seq[A])] = {
    def countedMaxSubsets(as: Seq[A], maxSize: Int): Stream[(Seq[A], Seq[A], Int)] =
      as match {
        case Nil => Stream((as, as, 0))
        case head +: rem =>
          (as.slice(0, 0), as, 0) #::
            (for {
              countedTail               <- countedMaxSubsets(rem, maxSize)
              (tail, complement, count) = countedTail
              result <- {
                if (count == maxSize)
                  Stream((tail, head +: complement, count))
                else if (tail.isEmpty)
                  Stream((head +: tail, complement, 1))
                else
                  Stream((tail, head +: complement, count), (head +: tail, complement, count + 1))
              }
            } yield result)
      }
    def worker(as: Seq[A], minSize: Int, maxSize: Int): Stream[(Seq[A], Seq[A], Int)] =
      if (maxSize < 0)
        Stream.empty
      else if (minSize > maxSize)
        Stream.empty
      else if (minSize <= 0)
        if (maxSize == 0)
          Stream((as.slice(0, 0), as, 0))
        else
          countedMaxSubsets(as, maxSize)
      else
        as match {
          case Nil => Stream.empty
          case hd +: rem =>
            val decr = minSize - 1
            for {
              countedTail               <- worker(rem, decr, maxSize)
              (tail, complement, count) = countedTail
              result <- {
                if (count == maxSize)
                  Stream((tail, hd +: complement, count))
                else if (count == decr)
                  Stream((hd +: tail, complement, minSize))
                else
                  Stream((tail, hd +: complement, count), (hd +: tail, complement, count + 1))
              }
            } yield result
        }
    worker(as, minSize, maxSize).map(x => (x._1, x._2))
  }

  // This helper function is useful in several productions
  def foldMatch[T, P](tlist: Seq[T], plist: Seq[P], remainder: Option[Var] = None)(
      implicit lft: HasLocallyFree[T],
      sm: SpatialMatcher[T, P]
  ): OptionalFreeMapWithCost[Seq[T]] =
    (tlist, plist) match {
      case (Nil, Nil) => OptionalFreeMapWithCost.pure(Nil)
      case (Nil, _) =>
        OptionalFreeMapWithCost.emptyMap[Seq[T]]
      case (trem, Nil) =>
        remainder match {
          case None =>
            OptionalFreeMapWithCost.emptyMap[Seq[T]]
          case Some(Var(FreeVar(level))) => {
            def freeCheck(trem: Seq[T], level: Int, acc: Seq[T]): OptionalFreeMapWithCost[Seq[T]] =
              trem match {
                case Nil => OptionalFreeMapWithCost.pure(acc)
                case item +: rem =>
                  if (lft.locallyFree(item, 0).isEmpty)
                    freeCheck(rem, level, acc :+ item)
                  else
                    OptionalFreeMapWithCost.emptyMap[Seq[T]]
              }
            freeCheck(trem, level, Vector.empty[T])
          }
          case Some(Var(Wildcard(_))) => OptionalFreeMapWithCost.pure(Nil)
          case _                      => OptionalFreeMapWithCost.emptyMap[Seq[T]]
        }
      case (t +: trem, p +: prem) =>
        spatialMatch(t, p).flatMap(_ => foldMatch(trem, prem, remainder))
    }

  def listMatchSingle[T](
      tlist: Seq[T],
      plist: Seq[T]
  )(implicit lf: HasLocallyFree[T], sm: SpatialMatcher[T, T]): OptionalFreeMapWithCost[Unit] =
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
  def listMatchSingle_[T](
      tlist: Seq[T],
      plist: Seq[T],
      merger: (Par, Seq[T]) => Par,
      remainder: Option[Int],
      wildcard: Boolean
  )(implicit lf: HasLocallyFree[T], sm: SpatialMatcher[T, T]): OptionalFreeMapWithCost[Unit] = {
    val exactMatch = !wildcard && remainder.isEmpty
    val plen       = plist.length
    val tlen       = tlist.length

    val result: OptionalFreeMapWithCost[Unit] =
      if (exactMatch && plen != tlen)
        OptionalFreeMapWithCost.emptyMap[Unit].charge(COMPARISON_COST)
      else if (plen > tlen)
        OptionalFreeMapWithCost.emptyMap[Unit].charge(COMPARISON_COST)
      else if (plen == 0 && tlen == 0 && remainder.isEmpty)
        OptionalFreeMapWithCost.pure(())
      else if (plen == 0 && remainder.isDefined) {
        val matchResult =
          if (tlist.forall(lf.locallyFree(_, 0).isEmpty))
            handleRemainder(tlist, remainder.get, merger)
          else
            OptionalFreeMapWithCost.emptyMap[Unit]
        matchResult.charge(COMPARISON_COST * tlist.size)
      } else
        listMatch(tlist, plist, merger, remainder, wildcard)

    result
  }

  private[this] def listMatch[T](
      targets: Seq[T],
      patterns: Seq[T],
      merger: (Par, Seq[T]) => Par,
      remainder: Option[Int],
      wildcard: Boolean
  )(implicit lf: HasLocallyFree[T], sm: SpatialMatcher[T, T]): OptionalFreeMapWithCost[Unit] = {

    sealed trait Pattern
    case class Term(term: T)         extends Pattern
    case class Remainder(level: Int) extends Pattern

    val remainderPatterns: Seq[Pattern] = remainder.fold(
      Seq.empty[Pattern]
    )(
      level => Seq.fill(targets.size - patterns.size)(Remainder(level))
    )
    val allPatterns = remainderPatterns ++ patterns.map(Term)

    val matchFunction: (Pattern, T) => OptionalFreeMapWithCost[Option[FreeMap]] =
      (pattern: Pattern, t: T) => {
        val matchEffect = pattern match {
          case Term(p) =>
            if (!lf.connectiveUsed(p)) {
              //match using `==` if pattern is a concrete term
              guard(t == p).charge(COMPARISON_COST)
            } else {
              spatialMatch(t, p)
            }
          case Remainder(_) =>
            //Remainders can't match non-concrete terms, because they can't be captured.
            //They match everything that's concrete though.
            guard(lf.locallyFree(t, 0).isEmpty).charge(COMPARISON_COST)
        }
        isolateState(matchEffect).attemptOpt
      }
    val maximumBipartiteMatch = MaximumBipartiteMatch(memoizeInHashMap(matchFunction))

    for {
      matchesOpt             <- maximumBipartiteMatch.findMatches(allPatterns, targets)
      matches                <- OptionalFreeMapWithCost.liftF(matchesOpt)
      freeMaps               = matches.map(_._3)
      updatedFreeMap         <- aggregateUpdates(freeMaps)
      _                      <- StateT.set[OptionWithCost, FreeMap](updatedFreeMap)
      remainderTargets       = matches.collect { case (target, _: Remainder, _) => target }
      remainderTargetsSet    = remainderTargets.toSet
      remainderTargetsSorted = targets.filter(remainderTargetsSet.contains)
      _ <- remainder match {
            case None =>
              // If there is a wildcard, we succeed.
              // We also succeed if there isn't but the remainder is empty.
              if (wildcard || remainderTargetsSorted.isEmpty)
                OptionalFreeMapWithCost.pure(())
              else
                // This should be prevented by the length checks.
                OptionalFreeMapWithCost.emptyMap
            // If there's a capture variable, we prefer to add things to that rather than throw them away.
            case Some(level) => {
              handleRemainder(remainderTargetsSorted, level, merger)
            }
          }
    } yield Unit
  }

  private def guard(predicate: => Boolean): OptionalFreeMapWithCost[Unit] =
    if (predicate) OptionalFreeMapWithCost.pure(()) else OptionalFreeMapWithCost.emptyMap

  private def isolateState[S, F[_]: Monad](f: StateT[F, S, _]): StateT[F, S, S] =
    for {
      initState   <- StateT.get[F, S]
      _           <- f
      resultState <- StateT.get[F, S]
      _           <- StateT.set[F, S](initState)
    } yield resultState

  private def memoizeInHashMap[A, B, C](f: (A, B) => C): (A, B) => C = {
    val memo = mutable.HashMap[(A, B), C]()
    (a, b) => memo.getOrElseUpdate((a, b), f(a, b))
  }

  private def aggregateUpdates(freeMaps: Seq[FreeMap]): OptionalFreeMapWithCost[FreeMap] =
    for {
      currentFreeMap <- StateT.get[OptionWithCost, FreeMap]
      _ <- guard {
            //The correctness of isolating MBM from changing FreeMap relies
            //on our ability to aggregate the var assignments from subsequent matches.
            //This means all the variables populated by MBM must not duplicate each other.
            //TODO start using MonadError in the interpreter and raise errors for violated assertions
            val currentVars = currentFreeMap.keys.toSet
            val addedVars   = freeMaps.flatMap(_.keys.filterNot(currentVars.contains))
            addedVars.size == addedVars.distinct.size
          }
      updatedFreeMap = freeMaps.fold(currentFreeMap)(_ ++ _)
    } yield updatedFreeMap

  private def handleRemainder[T](
      remainderTargets: Seq[T],
      level: Int,
      merger: (Par, Seq[T]) => Par
  ): OptionalFreeMapWithCost[Unit] =
    for {
      remainderPar <- StateT.inspect[OptionWithCost, FreeMap, Par](
                       (m: FreeMap) => m.getOrElse(level, VectorPar())
                     )
      //TODO: enforce sorted-ness of returned terms using types / by verifying the sorted-ness here
      remainderParUpdated = merger(remainderPar, remainderTargets)
      _ <- StateT.modify[OptionWithCost, FreeMap](
            (m: FreeMap) => m + (level -> remainderParUpdated)
          )
    } yield Unit

  case class ParCount(
      sends: Int = 0,
      receives: Int = 0,
      news: Int = 0,
      exprs: Int = 0,
      matches: Int = 0,
      ids: Int = 0,
      bundles: Int = 0
  ) {
    def min(other: ParCount): ParCount = binOp(math.min, other)

    def max(other: ParCount): ParCount = binOp(math.max, other)

    def +(other: ParCount): ParCount = binOp(saturatingAdd, other)

    def binOp(op: (Int, Int) => Int, other: ParCount): ParCount =
      ParCount(
        sends = op(sends, other.sends),
        receives = op(receives, other.receives),
        news = op(news, other.news),
        exprs = op(exprs, other.exprs),
        matches = op(matches, other.matches),
        ids = op(ids, other.ids),
        bundles = op(bundles, other.bundles)
      )

    // Only saturates going from positive to negative
    def saturatingAdd(l: Int, r: Int): Int = {
      val res = l + r
      (res | -(if (res < l) 1 else 0)) & ~Int.MinValue
    }
  }

  object ParCount {
    def apply(par: Par): ParCount =
      ParCount(
        sends = par.sends.size,
        receives = par.receives.size,
        news = par.news.size,
        matches = par.matches.size,
        exprs = par.exprs.size,
        ids = par.ids.size,
        bundles = par.bundles.size
      )
    def max: ParCount =
      ParCount(
        sends = Int.MaxValue,
        receives = Int.MaxValue,
        news = Int.MaxValue,
        matches = Int.MaxValue,
        exprs = Int.MaxValue,
        ids = Int.MaxValue,
        bundles = Int.MaxValue
      )

    def minMax(par: Par): (ParCount, ParCount) = {
      val pc = ParCount(noFrees(par))
      val wildcard: Boolean = par.exprs.exists { expr =>
        expr.exprInstance match {
          case EVarBody(EVar(v)) =>
            v.varInstance match {
              case Wildcard(_) => true
              case FreeVar(_)  => true
              case _           => false
            }
          case _ => false
        }
      }
      val minInit = pc
      val maxInit = if (wildcard) ParCount.max else pc

      par.connectives.foldLeft((minInit, maxInit)) {
        case ((min, max), con) =>
          val (cmin, cmax) = minMax(con)
          (min + cmin, max + cmax)
      }
    }

    def minMax(con: Connective): (ParCount, ParCount) =
      con.connectiveInstance match {
        case ConnAndBody(ConnectiveBody(ps)) =>
          val pMinMax = ps.map(minMax)
          (pMinMax.foldLeft(ParCount())(_ max _._1), pMinMax.foldLeft(ParCount.max)(_ min _._2))
        case ConnOrBody(ConnectiveBody(ps)) =>
          val pMinMax = ps.map(minMax)
          (pMinMax.foldLeft(ParCount.max)(_ min _._1), pMinMax.foldLeft(ParCount())(_ max _._2))
        case ConnNotBody(_) =>
          (ParCount(), ParCount.max)
        case ConnectiveInstance.Empty =>
          (ParCount(), ParCount())
        case _: VarRefBody =>
          // Variable references should be substituted before going into the matcher.
          // This should never happen.
          (ParCount(), ParCount())
        case _: ConnBool      => (ParCount(exprs = 1), ParCount(exprs = 1))
        case _: ConnInt       => (ParCount(exprs = 1), ParCount(exprs = 1))
        case _: ConnString    => (ParCount(exprs = 1), ParCount(exprs = 1))
        case _: ConnUri       => (ParCount(exprs = 1), ParCount(exprs = 1))
        case _: ConnByteArray => (ParCount(exprs = 1), ParCount(exprs = 1))
      }
  }
}

trait SpatialMatcherInstances {
  // This matches a single logical connective against a Par. If it can match
  // more ways than one, we don't care, as we care about what went into the
  // match, and not about how it matched inside. The tricky part goes into
  // the par/par matcher.
  implicit val connectiveMatcher: SpatialMatcher[Par, Connective] =
    fromFunction[Par, Connective] { (target, pattern) =>
      pattern.connectiveInstance match {
        case ConnAndBody(ConnectiveBody(ps)) =>
          ps.toList.traverse_(p => spatialMatch(target, p))
        case ConnOrBody(ConnectiveBody(ps)) => {
          def firstMatch(target: Par, patterns: Seq[Par]): OptionalFreeMapWithCost[Unit] =
            patterns match {
              case Nil => OptionalFreeMapWithCost.emptyMap
              case p +: rem =>
                OptionalFreeMapWithCost[Unit]((s: FreeMap) => {
                  OptionT(StateT((c: Cost) => {
                    spatialMatch(target, p).run(s).value.run(c).flatMap {
                      case (cost, None) =>
                        firstMatch(target, rem).run(s).value.run(cost)
                      case (cost, Some((_, _: Unit))) =>
                        Right((cost, Some((s, Unit))))
                    }
                  }))
                })
            }
          firstMatch(target, ps)
        }
        case ConnNotBody(p) =>
          OptionalFreeMapWithCost[Unit]((s: FreeMap) => {
            OptionT(StateT((c: Cost) => {
              spatialMatch(target, p).run(s).value.run(c).map {
                case (cost, None)         => (cost, Some((s, Unit)))
                case (cost, Some((_, _))) => (cost, None)
              }
            }))
          })
        case _: VarRefBody =>
          // this should never happen because variable references should be substituted
          OptionalFreeMapWithCost.emptyMap[Unit]

        case _: ConnBool =>
          target.singleExpr match {
            case Some(Expr(GBool(_))) =>
              OptionalFreeMapWithCost.pure(())
            case _ => OptionalFreeMapWithCost.emptyMap[Unit]
          }

        case _: ConnInt =>
          target.singleExpr match {
            case Some(Expr(GInt(_))) =>
              OptionalFreeMapWithCost.pure(())
            case _ => OptionalFreeMapWithCost.emptyMap[Unit]
          }

        case _: ConnString =>
          target.singleExpr match {
            case Some(Expr(GString(_))) =>
              OptionalFreeMapWithCost.pure(())
            case _ => OptionalFreeMapWithCost.emptyMap[Unit]
          }

        case _: ConnUri =>
          target.singleExpr match {
            case Some(Expr(GUri(_))) =>
              OptionalFreeMapWithCost.pure(())
            case _ => OptionalFreeMapWithCost.emptyMap[Unit]
          }

        case _: ConnByteArray =>
          target.singleExpr match {
            case Some(Expr(GByteArray(_))) =>
              OptionalFreeMapWithCost.pure(())
            case _ => OptionalFreeMapWithCost.emptyMap[Unit]
          }

        case ConnectiveInstance.Empty =>
          OptionalFreeMapWithCost.emptyMap[Unit]
      }
    }

  implicit val parSpatialMatcherInstance: SpatialMatcher[Par, Par] = fromNonDetFunction[Par, Par] {
    (target, pattern) =>
      if (!pattern.connectiveUsed) {
        val cost = equalityCheckCost(pattern, target)
        if (pattern == target)
          NonDetFreeMapWithCost.pure(()).charge(cost)
        else {
          NonDetFreeMapWithCost.emptyMap[Unit].charge(cost)
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
        ): NonDetFreeMapWithCost[Par] = {
          val (con, bounds, remainders) = labeledConnective
          for {
            sp <- NonDetFreeMapWithCost.liftF(
                   subPars(target, bounds._1, bounds._2, remainders._1, remainders._2)
                 )
            _ <- nonDetMatch(sp._1, con)
          } yield sp._2
        }
        for {
          remainder <- connectivesWithBounds.foldM(target)(matchConnectiveWithBounds)
          _ <- listMatchSingle_[Send](
                remainder.sends,
                pattern.sends,
                (p, s) => p.withSends(s),
                varLevel,
                wildcard
              ).toNonDet()
          _ <- listMatchSingle_[Receive](
                remainder.receives,
                pattern.receives,
                (p, s) => p.withReceives(s),
                varLevel,
                wildcard
              ).toNonDet()
          _ <- listMatchSingle_[New](
                remainder.news,
                pattern.news,
                (p, s) => p.withNews(s),
                varLevel,
                wildcard
              ).toNonDet()
          _ <- listMatchSingle_[Expr](
                remainder.exprs,
                noFrees(pattern.exprs),
                (p, e) => p.withExprs(e),
                varLevel,
                wildcard
              ).toNonDet()
          _ <- listMatchSingle_[Match](
                remainder.matches,
                pattern.matches,
                (p, e) => p.withMatches(e),
                varLevel,
                wildcard
              ).toNonDet()
          _ <- listMatchSingle_[Bundle](
                remainder.bundles,
                pattern.bundles,
                (p, b) => p.withBundles(b),
                varLevel,
                wildcard
              ).toNonDet()
          _ <- listMatchSingle_[GPrivate](
                remainder.ids,
                pattern.ids,
                (p, i) => p.withIds(i),
                varLevel,
                wildcard
              ).toNonDet()
        } yield Unit
      }
  }

  implicit val bundleSpatialMatcherInstance: SpatialMatcher[Bundle, Bundle] =
    fromFunction[Bundle, Bundle] { (target, pattern) =>
      val cost = equalityCheckCost(target, pattern)
      if (pattern == target)
        OptionalFreeMapWithCost.pure(()).charge(cost)
      else {
        OptionalFreeMapWithCost.emptyMap[Unit].charge(cost)
      }
    }

  implicit val sendSpatialMatcherInstance: SpatialMatcher[Send, Send] = fromFunction[Send, Send] {
    (target, pattern) =>
      if (target.persistent != pattern.persistent)
        OptionalFreeMapWithCost.emptyMap[Unit].charge(COMPARISON_COST)
      else
        for {
          _ <- spatialMatch(target.chan, pattern.chan)
          _ <- foldMatch(target.data, pattern.data)
        } yield Unit
  }

  implicit val receiveSpatialMatcherInstance: SpatialMatcher[Receive, Receive] =
    fromFunction[Receive, Receive] { (target, pattern) =>
      if (target.persistent != pattern.persistent)
        OptionalFreeMapWithCost.emptyMap[Unit].charge(COMPARISON_COST)
      else
        for {
          _ <- listMatchSingle[ReceiveBind](target.binds, pattern.binds)
          _ <- spatialMatch(target.body, pattern.body)
        } yield Unit
    }

  implicit val newSpatialMatcherInstance: SpatialMatcher[New, New] = fromFunction[New, New] {
    (target, pattern) =>
      if (target.bindCount == pattern.bindCount)
        spatialMatch(target.p, pattern.p).charge(COMPARISON_COST)
      else
        OptionalFreeMapWithCost.emptyMap[Unit].charge(COMPARISON_COST)
  }

  implicit val exprSpatialMatcherInstance: SpatialMatcher[Expr, Expr] = fromFunction[Expr, Expr] {
    (target, pattern) =>
      (target.exprInstance, pattern.exprInstance) match {
        case (EListBody(EList(tlist, _, _, _)), EListBody(EList(plist, _, _, rem))) => {
          for {
            matchedRem <- foldMatch(tlist, plist, rem)
            _ <- rem match {
                  case Some(Var(FreeVar(level))) =>
                    StateT.modify[OptionWithCost, FreeMap](m => m + (level -> EList(matchedRem)))
                  case _ => OptionalFreeMapWithCost.pure[Unit](())
                }
          } yield Unit
        }
        case (ETupleBody(ETuple(tlist, _, _)), ETupleBody(ETuple(plist, _, _))) => {
          foldMatch(tlist, plist).map(_ => Unit)
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
            OptionalFreeMapWithCost.pure(()).charge(cost)
          else
            OptionalFreeMapWithCost.emptyMap[Unit].charge(cost)
        case (ENotBody(ENot(t)), ENotBody(ENot(p))) => spatialMatch(t, p)
        case (ENegBody(ENeg(t)), ENegBody(ENeg(p))) => spatialMatch(t, p)
        case (EMultBody(EMult(t1, t2)), EMultBody(EMult(p1, p2))) =>
          for {
            _ <- spatialMatch(t1, p1)
            _ <- spatialMatch(t2, p2)
          } yield Unit
        case (EDivBody(EDiv(t1, t2)), EDivBody(EDiv(p1, p2))) =>
          for {
            _ <- spatialMatch(t1, p1)
            _ <- spatialMatch(t2, p2)
          } yield Unit
        case (
            EPercentPercentBody(EPercentPercent(t1, t2)),
            EPercentPercentBody(EPercentPercent(p1, p2))
            ) =>
          for {
            _ <- spatialMatch(t1, p1)
            _ <- spatialMatch(t2, p2)
          } yield Unit
        case (EPlusBody(EPlus(t1, t2)), EPlusBody(EPlus(p1, p2))) =>
          for {
            _ <- spatialMatch(t1, p1)
            _ <- spatialMatch(t2, p2)
          } yield Unit
        case (EPlusPlusBody(EPlusPlus(t1, t2)), EPlusPlusBody(EPlusPlus(p1, p2))) =>
          for {
            _ <- spatialMatch(t1, p1)
            _ <- spatialMatch(t2, p2)
          } yield Unit
        case (EMinusMinusBody(EMinusMinus(t1, t2)), EMinusMinusBody(EMinusMinus(p1, p2))) =>
          for {
            _ <- spatialMatch(t1, p1)
            _ <- spatialMatch(t2, p2)
          } yield Unit
        case _ => OptionalFreeMapWithCost.emptyMap[Unit]
      }
  }

  implicit val matchSpatialMatcherInstance: SpatialMatcher[Match, Match] =
    fromFunction[Match, Match] { (target, pattern) =>
      for {
        _ <- spatialMatch(target.target, pattern.target)
        _ <- foldMatch(target.cases, pattern.cases)
      } yield Unit
    }

  /**
    * Note that currently there should be no way to put a GPrivate in a pattern
    * because patterns start with an empty environment.
    * We're going to write the obvious definition anyway.
    */
  implicit val gprivateSpatialMatcherInstance: SpatialMatcher[GPrivate, GPrivate] =
    fromFunction[GPrivate, GPrivate] { (target, pattern) =>
      if (target == pattern) {
        val cost = equalityCheckCost(target, pattern)
        OptionalFreeMapWithCost.pure(()).charge(cost)
      } else
        OptionalFreeMapWithCost.emptyMap
    }

  implicit val receiveBindSpatialMatcherInstance: SpatialMatcher[ReceiveBind, ReceiveBind] =
    fromFunction[ReceiveBind, ReceiveBind] { (target, pattern) =>
      if (target.patterns != pattern.patterns) {
        val cost: Cost =
          target.patterns
            .zip(pattern.patterns)
            .map(x => equalityCheckCost(x._1, x._2))
            .foldLeft(Cost(0))(_ + _)
        OptionalFreeMapWithCost.emptyMap.charge(cost)
      } else
        spatialMatch(target.source, pattern.source)
    }

  implicit val matchCaseSpatialMatcherInstance: SpatialMatcher[MatchCase, MatchCase] =
    fromFunction[MatchCase, MatchCase] { (target, pattern) =>
      if (target.pattern != pattern.pattern) {
        val cost: Cost = equalityCheckCost(target.pattern, pattern.pattern)
        OptionalFreeMapWithCost.emptyMap.charge(cost)
      } else
        spatialMatch(target.source, pattern.source)
    }

}
