package coop.rchain.rholang.interpreter.matcher

import cats.arrow.FunctionK
import cats.data.{OptionT, State, StateT}
import cats.implicits._
import cats.{Eval => _}
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Connective.ConnectiveInstance.{ConnAndBody, ConnNotBody, ConnOrBody, VarRefBody}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits.{VectorPar, _}
import coop.rchain.rholang.interpreter.accounting.{CostAccount, _}
import coop.rchain.rholang.interpreter.matcher.NonDetFreeMapWithCost._
import coop.rchain.rholang.interpreter.matcher.OptionalFreeMapWithCost._
import coop.rchain.rholang.interpreter.matcher.SpatialMatcher._
import coop.rchain.rholang.interpreter.matcher.StreamT._

import scala.annotation.tailrec
import scala.collection.immutable.Stream

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
  def spatialMatch[T, P](target: T, pattern: P)(
      implicit sm: SpatialMatcher[T, P]): OptionalFreeMapWithCost[Unit] =
    SpatialMatcher[T, P].spatialMatch(target, pattern)

  def nonDetMatch[T, P](target: T, pattern: P)(
      implicit sm: SpatialMatcher[T, P]): NonDetFreeMapWithCost[Unit] =
    SpatialMatcher[T, P].nonDetMatch(target, pattern)

  def apply[T, P](implicit sm: SpatialMatcher[T, P]) = sm

  def fromFunction[T, P](fn: (T, P) => OptionalFreeMapWithCost[Unit]): SpatialMatcher[T, P] =
    new SpatialMatcher[T, P] {
      override def spatialMatch(target: T, pattern: P): OptionalFreeMapWithCost[Unit] =
        fn(target, pattern)
      override def nonDetMatch(target: T, pattern: P): NonDetFreeMapWithCost[Unit] =
        fn(target, pattern).mapK[StreamT[State[CostAccount, ?], ?]](
          new FunctionK[OptionT[State[CostAccount, ?], ?], StreamT[State[CostAccount, ?], ?]] {
            override def apply[A](
                fa: OptionT[State[CostAccount, ?], A]): StreamT[State[CostAccount, ?], A] =
              StreamT(fa.fold(Stream.empty[A])(single => Stream(single)))
          })
    }

  def fromNonDetFunction[T, P](fn: (T, P) => NonDetFreeMapWithCost[Unit]): SpatialMatcher[T, P] =
    new SpatialMatcher[T, P] {
      override def nonDetMatch(target: T, pattern: P): NonDetFreeMapWithCost[Unit] =
        fn(target, pattern)
      override def spatialMatch(target: T, pattern: P): OptionalFreeMapWithCost[Unit] =
        fn(target, pattern).mapK[OptionT[State[CostAccount, ?], ?]](
          new FunctionK[StreamT[State[CostAccount, ?], ?], OptionT[State[CostAccount, ?], ?]] {
            override def apply[A](
                fa: StreamT[State[CostAccount, ?], A]): OptionT[State[CostAccount, ?], A] =
              OptionT(fa.value.map(_.headOption))
          })
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

  def subPars(par: Par,
              min: ParCount,
              max: ParCount,
              minPrune: ParCount,
              maxPrune: ParCount): Stream[(Par, Par)] = {

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
      (Par(subSends._1,
           subReceives._1,
           subNews._1,
           subExprs._1,
           subMatches._1,
           subIds._1,
           subBundles._1),
       Par(subSends._2,
           subReceives._2,
           subNews._2,
           subExprs._2,
           subMatches._2,
           subIds._2,
           subBundles._2))
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
      sm: SpatialMatcher[T, P]): OptionalFreeMapWithCost[Seq[T]] =
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

  /** This function finds a single matching from a list of patterns and a list of targets.
    * Any remaining terms are either grouped with the free variable varLevel or thrown away with the wildcard.
    * If both are provided, we prefer to capture terms that can be captured.
    *
    * @param tlist  the target list
    * @param plist  the pattern list
    * @param merger a function that adds a captured T to a par. Used for updating the state map.
    * @param varLevel if non-empty, the free variable level where to put the remaining T's
    * @param wildcard if true, there is a wildcard in parallel with the pattern list.
    * @param lf
    * @param sm a function that does a spatial match between T's
    * @tparam T
    * @return
    */
  def listMatchSingleNonDet[T](tlist: Seq[T],
                               plist: Seq[T],
                               merger: (Par, T) => Par,
                               varLevel: Option[Int],
                               wildcard: Boolean)(
      implicit lf: HasLocallyFree[T],
      sm: SpatialMatcher[T, T]): NonDetFreeMapWithCost[Unit] = {
    val exactMatch = !wildcard && varLevel.isEmpty
    val plen       = plist.length
    val tlen       = tlist.length

    val result: NonDetFreeMapWithCost[Unit] =
      if (exactMatch && plen != tlen)
        NonDetFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(COMPARISON_COST))
      else if (plen > tlen)
        NonDetFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(COMPARISON_COST))
      else
        listMatch(tlist, plist, merger, varLevel, wildcard)
          .transformF[StreamT[State[CostAccount, ?], ?], Unit, FreeMap](streamT =>
            StreamT(streamT.value.map[Stream[(FreeMap, Unit)]] {
              case None       => Stream.empty
              case Some(head) => Stream(head)
            }))

    result
  }

  def listMatchSingle[T](tlist: Seq[T], plist: Seq[T])(
      implicit lf: HasLocallyFree[T],
      sm: SpatialMatcher[T, T]): OptionalFreeMapWithCost[Unit] =
    listMatchSingleNonDet(tlist, plist, (p: Par, _: T) => p, None, false)
      .mapK[OptionT[State[CostAccount, ?], ?]](
        new FunctionK[StreamT[State[CostAccount, ?], ?], OptionT[State[CostAccount, ?], ?]] {
          override def apply[A](
              fa: StreamT[State[CostAccount, ?], A]): OptionT[State[CostAccount, ?], A] =
            OptionT(fa.value.map(_.headOption))
        })

  private[this] def listMatch[T](tlist: Seq[T],
                                 plist: Seq[T],
                                 merger: (Par, T) => Par,
                                 varLevel: Option[Int],
                                 wildcard: Boolean)(
      implicit lf: HasLocallyFree[T],
      sm: SpatialMatcher[T, T]): OptionalFreeMapWithCost[Unit] = {

    type Pattern = (T, Stream[OptionalFreeMapWithCost[T]])

    //TODO enforce sorted-ness of targets and patterns.
    //Correctness of the matching relies on them being sorted.
    //Use a type class? A type tag?
    val termsSorted    = tlist
    val patternsSorted = plist.zip(Stream.continually(termsSorted))

    def guard(predicate: => Boolean): OptionalFreeMapWithCost[Unit] =
      if (predicate) OptionalFreeMapWithCost.pure(()) else OptionalFreeMapWithCost.emptyMap

    val maximumBipartiteMatch = MaximumBipartiteMatch((p: T, t: T) => {
      val matchEffect = if (!lf.connectiveUsed(p)) {
        guard(t == p).modifyCost(_.charge(COMPARISON_COST))
      } else {
        spatialMatch(t, p)
      }
      matchEffect.attempt.map(_.isRight)
    })

    for {
      matchedTargetsOpt <- maximumBipartiteMatch.findMatches(patternsSorted.toList)
      matchedTargets <- matchedTargetsOpt.fold(
                         OptionalFreeMapWithCost.emptyMap[Seq[T]]
                       )(
                         OptionalFreeMapWithCost.pure[Seq[T]]
                       )
      remainderTargets = termsSorted.filterNot(matchedTargets.contains)
      _ <- varLevel match {
            case None =>
              // If there is a wildcard, we succeed.
              // We also succeed if there isn't but the remainder is empty.
              if (wildcard || remainderTargets.isEmpty)
                OptionalFreeMapWithCost.pure(())
              else
                // This should be prevented by the length checks.
                OptionalFreeMapWithCost.emptyMap
            // If there's a capture variable, we prefer to add things to that rather than throw them away.
            case Some(level) => {
              /*
              Matching a single variable with a single term has so far been handled
              by matching a list of 0 patterns with a list of 1 target, with a remainder present.

              In general, matching remainders requires combining/merging all terms matched to the remainder
              on a single capture variable. E.g. `1 | 2 | 3 matches 2 | x` would return x = 1 | 3.
              See the `handleReminder` method below and notice that initial state for the fold is taken from
              previous matches.

              Matching a single variable using that mechanism (e.g. [1] matches [x]) was OK so far, since
              overriding the match state (FreeMap entry) for a given variable never happened in the previous algo.

              We don't have a proof it was impossible. What we know for sure though is:
              none of the tests added for the new algo - which revealed this problem - have failed on the old algo.

              Since we've started modifying the var assignment during the MBM algorithm, the case of matching a var
              to a single term must be handled separately, without merging the prior match state.
              */
              if (patternsSorted.isEmpty && remainderTargets.length == 1 &&
                  lf.locallyFree(remainderTargets.head, 0).isEmpty) {
                StateT.modify[OptionT[State[CostAccount, ?], ?], FreeMap]((m: FreeMap) =>
                  m + (level -> merger(VectorPar(), remainderTargets.head)))
              } else {
                handleRemainder(remainderTargets, level, merger, wildcard)
              }
            }
          }
    } yield Unit
  }

  private def handleRemainder[T](rem: Seq[T],
                                 level: Int,
                                 merger: (Par, T) => Par,
                                 wildcard: Boolean)(
      implicit lf: HasLocallyFree[T],
      sm: SpatialMatcher[T, T]): OptionalFreeMapWithCost[Unit] = {
    // This function is essentially an early terminating left fold.
    @tailrec
    def foldRemainder(remainder: Seq[T], p: Par): OptionalFreeMapWithCost[Par] =
      remainder match {
        case Nil => OptionalFreeMapWithCost.pure(p)
        case item +: rem =>
          if (lf.locallyFree(item, 0).isEmpty)
            foldRemainder(rem, merger(p, item))
          else if (wildcard)
            foldRemainder(rem, p)
          else
            OptionalFreeMapWithCost.emptyMap[Par]
      }
    for {
      p <- StateT.inspect[OptionT[State[CostAccount, ?], ?], FreeMap, Par]((m: FreeMap) =>
            m.getOrElse(level, VectorPar()))
      //TODO: shouldn't we return sorted terms?
      collectPar <- foldRemainder(rem.reverse, p)
      _ <- StateT.modify[OptionT[State[CostAccount, ?], ?], FreeMap]((m: FreeMap) =>
            m + (level -> collectPar))
    } yield Unit
  }

  case class ParCount(sends: Int = 0,
                      receives: Int = 0,
                      news: Int = 0,
                      exprs: Int = 0,
                      matches: Int = 0,
                      ids: Int = 0,
                      bundles: Int = 0) {
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
                  OptionT(State((c: CostAccount) => {
                    spatialMatch(target, p).run(s).value.run(c).value match {
                      case (cost, None) =>
                        (cost, firstMatch(target, rem).run(s).value.run(c).value._2)
                      case (cost, Some((_, _: Unit))) => (cost, Some((s, Unit)))
                    }
                  }))
                })
            }
          firstMatch(target, ps)
        }
        case ConnNotBody(p) =>
          OptionalFreeMapWithCost[Unit]((s: FreeMap) => {
            OptionT(State((c: CostAccount) => {
              spatialMatch(target, p).run(s).value.run(c).value match {
                case (cost, None)         => (cost, Some((s, Unit)))
                case (cost, Some((_, _))) => (cost, None)
              }
            }))
          })
        case _: VarRefBody =>
          // this should never happen because variable references should be substituted
          OptionalFreeMapWithCost.emptyMap[Unit]

        case ConnectiveInstance.Empty =>
          OptionalFreeMapWithCost.emptyMap[Unit]
      }
    }

  implicit val parSpatialMatcherInstance: SpatialMatcher[Par, Par] = fromNonDetFunction[Par, Par] {
    (target, pattern) =>
      if (!pattern.connectiveUsed) {
        val cost = equalityCheckCost(pattern, target)
        if (pattern == target)
          NonDetFreeMapWithCost.pure(()).modifyCost(_.charge(cost))
        else {
          NonDetFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(cost))
        }
      } else {

        @tailrec
        def possiblyFind[T, R](prop: T => Option[R], haystack: Seq[T]): Option[R] =
          haystack match {
            case Nil => None
            case head +: rest =>
              prop(head) match {
                case None  => possiblyFind(prop, rest)
                case found => found
              }
          }

        val varLevel: Option[Int] = possiblyFind[Expr, Int](
          {
            case expr =>
              expr.exprInstance match {
                case EVarBody(EVar(v)) =>
                  v.varInstance match {
                    case FreeVar(level) => Some(level)
                    case _              => None
                  }
                case _ => None
              }
          },
          pattern.exprs
        )

        val wildcard: Boolean = pattern.exprs.exists { expr =>
          expr.exprInstance match {
            case EVarBody(EVar(v)) =>
              v.varInstance match {
                case Wildcard(_) => true
                case _           => false
              }
            case _ => false
          }
        }

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
            labeledConnective: (Connective, (ParCount, ParCount), (ParCount, ParCount)))
          : NonDetFreeMapWithCost[Par] = {
          val (con, bounds, remainders) = labeledConnective
          for {
            sp <- NonDetFreeMapWithCost.liftF(
                   subPars(target, bounds._1, bounds._2, remainders._1, remainders._2))
            _ <- nonDetMatch(sp._1, con)
          } yield sp._2
        }
        for {
          remainder <- connectivesWithBounds.foldM(target)(matchConnectiveWithBounds)
          _ <- listMatchSingleNonDet[Send](remainder.sends,
                                           pattern.sends,
                                           (p, s) => p.withSends(s +: p.sends),
                                           varLevel,
                                           wildcard)
          _ <- listMatchSingleNonDet[Receive](remainder.receives,
                                              pattern.receives,
                                              (p, s) => p.withReceives(s +: p.receives),
                                              varLevel,
                                              wildcard)
          _ <- listMatchSingleNonDet[New](remainder.news,
                                          pattern.news,
                                          (p, s) => p.withNews(s +: p.news),
                                          varLevel,
                                          wildcard)
          _ <- listMatchSingleNonDet[Expr](remainder.exprs,
                                           noFrees(pattern.exprs),
                                           (p, e) => p.withExprs(e +: p.exprs),
                                           varLevel,
                                           wildcard)
          _ <- listMatchSingleNonDet[Match](remainder.matches,
                                            pattern.matches,
                                            (p, e) => p.withMatches(e +: p.matches),
                                            varLevel,
                                            wildcard)
          _ <- listMatchSingleNonDet[Bundle](remainder.bundles,
                                             pattern.bundles,
                                             (p, b) => p.withBundles(b +: p.bundles),
                                             varLevel,
                                             wildcard)
          _ <- listMatchSingleNonDet[GPrivate](remainder.ids,
                                               pattern.ids,
                                               (p, i) => p.withIds(i +: p.ids),
                                               varLevel,
                                               wildcard)
        } yield Unit
      }
  }

  implicit val bundleSpatialMatcherInstance: SpatialMatcher[Bundle, Bundle] =
    fromFunction[Bundle, Bundle] { (target, pattern) =>
      val cost = equalityCheckCost(target, pattern)
      if (pattern == target)
        OptionalFreeMapWithCost.pure(()).modifyCost(_.charge(cost))
      else {
        OptionalFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(cost))
      }
    }

  implicit val sendSpatialMatcherInstance: SpatialMatcher[Send, Send] = fromFunction[Send, Send] {
    (target, pattern) =>
      if (target.persistent != pattern.persistent)
        OptionalFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(COMPARISON_COST))
      else
        for {
          _ <- spatialMatch(target.chan, pattern.chan)
          _ <- foldMatch(target.data, pattern.data)
        } yield Unit
  }

  implicit val receiveSpatialMatcherInstance: SpatialMatcher[Receive, Receive] =
    fromFunction[Receive, Receive] { (target, pattern) =>
      if (target.persistent != pattern.persistent)
        OptionalFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(COMPARISON_COST))
      else
        for {
          _ <- listMatchSingle[ReceiveBind](target.binds, pattern.binds)
          _ <- spatialMatch(target.body, pattern.body)
        } yield Unit
    }

  implicit val newSpatialMatcherInstance: SpatialMatcher[New, New] = fromFunction[New, New] {
    (target, pattern) =>
      if (target.bindCount == pattern.bindCount)
        spatialMatch(target.p, pattern.p).modifyCost(_.charge(COMPARISON_COST))
      else
        OptionalFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(COMPARISON_COST))
  }

  implicit val exprSpatialMatcherInstance: SpatialMatcher[Expr, Expr] = fromFunction[Expr, Expr] {
    (target, pattern) =>
      (target.exprInstance, pattern.exprInstance) match {
        case (EListBody(EList(tlist, _, _, _)), EListBody(EList(plist, _, _, rem))) => {
          for {
            matchedRem <- foldMatch(tlist, plist, rem)
            _ <- rem match {
                  case Some(Var(FreeVar(level))) =>
                    StateT.modify[OptionT[State[CostAccount, ?], ?], FreeMap](m =>
                      m + (level -> EList(matchedRem)))
                  case _ => OptionalFreeMapWithCost.pure[Unit](())
                }
          } yield Unit
        }
        case (ETupleBody(ETuple(tlist, _, _)), ETupleBody(ETuple(plist, _, _))) => {
          foldMatch(tlist, plist).map(_ => Unit)
        }
        case (EVarBody(EVar(vp)), EVarBody(EVar(vt))) =>
          val cost = equalityCheckCost(vp, vt)
          if (vp == vt)
            OptionalFreeMapWithCost.pure(()).modifyCost(_.charge(cost))
          else
            OptionalFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(cost))
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
        case (EPercentPercentBody(EPercentPercent(t1, t2)),
              EPercentPercentBody(EPercentPercent(p1, p2))) =>
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
        case (EEvalBody(chan1), EEvalBody(chan2)) =>
          spatialMatch(chan1, chan2)
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
        OptionalFreeMapWithCost.pure(()).modifyCost(_.charge(cost))
      } else
        OptionalFreeMapWithCost.emptyMap
    }

  implicit val channelSpatialMatcherInstance: SpatialMatcher[Channel, Channel] =
    fromFunction[Channel, Channel] { (target, pattern) =>
      (target.channelInstance, pattern.channelInstance) match {
        case (_, ChanVar(v)) if v.varInstance.isWildcard => OptionalFreeMapWithCost.pure(())
        case (Quote(p), ChanVar(v)) => {
          v.varInstance match {
            case FreeVar(level) => {
              if (p.locallyFree.isEmpty) StateT.modify(m => m + (level -> p))
              else OptionalFreeMapWithCost.emptyMap
            }
            case _ => OptionalFreeMapWithCost.emptyMap
          }
        }
        case (ChanVar(tv), ChanVar(pv)) =>
          (tv.varInstance, pv.varInstance) match {
            case (BoundVar(tlevel), BoundVar(plevel)) => {
              if (tlevel === plevel)
                OptionalFreeMapWithCost.pure(()).modifyCost(_.charge(COMPARISON_COST))
              else OptionalFreeMapWithCost.emptyMap[Unit].modifyCost(_.charge(COMPARISON_COST))
            }
            case _ => OptionalFreeMapWithCost.emptyMap
          }
        case (Quote(tproc), Quote(pproc)) => spatialMatch(tproc, pproc)
        case _                            => OptionalFreeMapWithCost.emptyMap
      }
    }

  implicit val receiveBindSpatialMatcherInstance: SpatialMatcher[ReceiveBind, ReceiveBind] =
    fromFunction[ReceiveBind, ReceiveBind] { (target, pattern) =>
      if (target.patterns != pattern.patterns) {
        val cost: Cost =
          target.patterns
            .zip(pattern.patterns)
            .map(x => equalityCheckCost(x._1, x._2))
            .sum
        OptionalFreeMapWithCost.emptyMap.modifyCost(_.charge(cost))
      } else
        spatialMatch(target.source, pattern.source)
    }

  implicit val matchCaseSpatialMatcherInstance: SpatialMatcher[MatchCase, MatchCase] =
    fromFunction[MatchCase, MatchCase] { (target, pattern) =>
      if (target.pattern != pattern.pattern) {
        val cost: Cost = equalityCheckCost(target.pattern, pattern.pattern)
        OptionalFreeMapWithCost.emptyMap.modifyCost(_.charge(cost))
      } else
        spatialMatch(target.source, pattern.source)
    }

}
