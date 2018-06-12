package coop.rchain.rholang.interpreter

import cats.data._
import cats.implicits._
import cats.{Eval => _}
import cats.Foldable
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.SpatialMatcher.OptionalFreeMap
import coop.rchain.rholang.interpreter.SpatialMatcher.NonDetFreeMap
import coop.rchain.models.rholang.implicits.{
  fromEList,
  fromExpr,
  BundleLocallyFree,
  ExprLocallyFree,
  GPrivateLocallyFree,
  MatchCaseLocallyFree,
  MatchLocallyFree,
  NewLocallyFree,
  ParLocallyFree,
  ReceiveBindLocallyFree,
  ReceiveLocallyFree,
  SendLocallyFree,
  VectorPar
}

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
  def spatialMatch(target: T, pattern: P): OptionalFreeMap[Unit]
  def nonDetMatch(target: T, pattern: P): NonDetFreeMap[Unit]
}

object SpatialMatcher {
  def apply[T, P](implicit sm: SpatialMatcher[T, P]) = sm

  def spatialMatch[T, P](target: T, pattern: P)(
      implicit sm: SpatialMatcher[T, P]): OptionalFreeMap[Unit] =
    SpatialMatcher[T, P].spatialMatch(target, pattern)

  def nonDetMatch[T, P](target: T, pattern: P)(
      implicit sm: SpatialMatcher[T, P]): NonDetFreeMap[Unit] =
    SpatialMatcher[T, P].nonDetMatch(target, pattern)

  def fromFunction[T, P](fn: (T, P) => OptionalFreeMap[Unit]): SpatialMatcher[T, P] =
    new SpatialMatcher[T, P] {
      override def spatialMatch(target: T, pattern: P): OptionalFreeMap[Unit] =
        fn(target, pattern)
      override def nonDetMatch(target: T, pattern: P): NonDetFreeMap[Unit] =
        StateT((s: FreeMap) => {
          fn(target, pattern).run(s) match {
            case None         => Stream.empty
            case Some(single) => Stream(single)
          }
        })
    }

  def fromNonDetFunction[T, P](fn: (T, P) => NonDetFreeMap[Unit]): SpatialMatcher[T, P] =
    new SpatialMatcher[T, P] {
      override def nonDetMatch(target: T, pattern: P): NonDetFreeMap[Unit] =
        fn(target, pattern)
      override def spatialMatch(target: T, pattern: P): OptionalFreeMap[Unit] =
        StateT((s: FreeMap) => {
          fn(target, pattern).run(s) match {
            case Stream.Empty => None
            case single #:: _ => Some(single)
          }
        })
    }

  type FreeMap            = Map[Int, Par]
  type OptionalFreeMap[A] = StateT[Option, FreeMap, A]
  type NonDetFreeMap[A]   = StateT[Stream, FreeMap, A]

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

  def emptyMap: FreeMap = Map.empty[Int, Par]

  case class ParCount(sends: Int = 0,
                      receives: Int = 0,
                      news: Int = 0,
                      exprs: Int = 0,
                      matches: Int = 0,
                      ids: Int = 0,
                      bundles: Int = 0) {
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
    def min(other: ParCount): ParCount = binOp(math.min, other)
    def max(other: ParCount): ParCount = binOp(math.max, other)
    def +(other: ParCount): ParCount   = binOp(saturatingAdd, other)
    // Only saturates going from positive to negative
    def saturatingAdd(l: Int, r: Int): Int = {
      val res = l + r
      (res | -(if (res < l) 1 else 0)) & ~Int.MinValue
    }
  }

  private[this] def noFrees(exprs: Seq[Expr]): Seq[Expr] =
    exprs.filter({ (expr) =>
      expr.exprInstance match {
        case EVarBody(EVar((v))) =>
          v.varInstance match {
            case FreeVar(_)  => false
            case Wildcard(_) => false
            case _           => true
          }
        case _ => true
      }
    })

  private[this] def noFrees(par: Par): Par =
    par.withExprs(noFrees(par.exprs))

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
      }
  }

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
      sm: SpatialMatcher[T, P]): OptionalFreeMap[Seq[T]] =
    (tlist, plist) match {
      case (Nil, Nil) => StateT.pure(Nil)
      case (Nil, _)   => StateT.liftF[Option, FreeMap, Seq[T]](None)
      case (trem, Nil) =>
        remainder match {
          case None => StateT.liftF[Option, FreeMap, Seq[T]](None)
          case Some(Var(FreeVar(level))) => {
            def freeCheck(trem: Seq[T], level: Int, acc: Seq[T]): OptionalFreeMap[Seq[T]] =
              trem match {
                case Nil => StateT.pure(acc)
                case item +: rem =>
                  if (lft.locallyFree(item).isEmpty)
                    freeCheck(rem, level, acc :+ item)
                  else
                    StateT.liftF(None)
              }
            freeCheck(trem, level, Vector.empty[T])
          }
          case Some(Var(Wildcard(_))) => StateT.pure(Nil)
          case _                      => StateT.liftF[Option, FreeMap, Seq[T]](None)
        }
      case (t +: trem, p +: prem) =>
        spatialMatch(t, p).flatMap(_ => foldMatch(trem, prem, remainder))
    }

  // This function finds a single matching from a list of patterns and a list of
  // targets.
  // Any remaining terms are either grouped with the free variable varLevel or
  // thrown away with the wildcard. If both are provided, we prefer to capture
  // terms that can be captured.
  // tlist is the target list
  // plist is the pattern list
  // matcher is a function that does a spatial match between T's
  // merger is a function that adds a captured T to a par. Used for updating the
  //   state map.
  // varLevel: if non-empty, the free variable level where to put the remaining
  //   T's
  // wildcard: if true, there is a wildcard in parallel with the pattern list.
  private[this] def listMatchSingleNonDet[T](tlist: Seq[T],
                                             plist: Seq[T],
                                             merger: (Par, T) => Par,
                                             varLevel: Option[Int],
                                             wildcard: Boolean)(
      implicit lf: HasLocallyFree[T],
      sm: SpatialMatcher[T, T]): NonDetFreeMap[Unit] = {
    val exactMatch = !wildcard && varLevel.isEmpty
    val plen       = plist.length
    val tlen       = tlist.length
    if (exactMatch && plen != tlen)
      StateT.liftF(None)
    else if (plen > tlen)
      StateT.liftF(None)
    // This boundary is very similar to Oleg's once.
    StateT((s: FreeMap) => {
      listMatch(tlist, plist, merger, varLevel, wildcard).run(s) match {
        case Stream.Empty => Stream.Empty
        case head #:: _   => Stream(head)
      }
    })
  }

  private[this] def listMatchSingle[T](tlist: Seq[T],
                                       plist: Seq[T],
                                       merger: (Par, T) => Par,
                                       varLevel: Option[Int],
                                       wildcard: Boolean)(
      implicit lf: HasLocallyFree[T],
      sm: SpatialMatcher[T, T]): OptionalFreeMap[Unit] =
    StateT((s: FreeMap) => {
      listMatchSingleNonDet(tlist, plist, merger, varLevel, wildcard).run(s) match {
        case Stream.Empty => None
        case head #:: _   => Some(head)
      }
    })

  private[this] def possiblyRemove[T](needle: T, haystack: Seq[T]): Option[Seq[T]] = {
    val (before, after) = haystack.span(x => x != needle)
    after match {
      case Nil       => None
      case _ +: tail => Some(before ++ tail)
    }
  }

  private[this] def listMatch[T](tlist: Seq[T],
                                 plist: Seq[T],
                                 merger: (Par, T) => Par,
                                 varLevel: Option[Int],
                                 wildcard: Boolean)(implicit lf: HasLocallyFree[T],
                                                    sm: SpatialMatcher[T, T]): NonDetFreeMap[Unit] =
    (tlist, plist) match {
      // Handle the remainder.
      case (rem, Nil) =>
        varLevel match {
          case None =>
            // If there is a wildcard, we succeed.
            // We also succeed if there isn't but the remainder is empty.
            if (wildcard || rem == Nil)
              StateT.pure(Unit)
            else
              // This should be prevented by the length checks.
              StateT.liftF(Stream.Empty)
          // If there's a capture variable, we prefer to add things to that rather than throw them
          // away.
          case Some(level) => {
            // This function is essentially an early terminating left fold.
            @tailrec
            def foldRemainder(remainder: Seq[T], p: Par): NonDetFreeMap[Par] =
              remainder match {
                case Nil => StateT.pure(p)
                case item +: rem =>
                  if (lf.locallyFree(item).isEmpty)
                    foldRemainder(rem, merger(p, item))
                  else if (wildcard)
                    foldRemainder(rem, p)
                  else
                    StateT.liftF(Stream.Empty)
              }
            for {
              p <- StateT.inspect[Stream, FreeMap, Par]((m: FreeMap) =>
                    m.getOrElse(level, VectorPar()))
              collectPar <- foldRemainder(rem, p)
              _          <- StateT.modify[Stream, FreeMap]((m: FreeMap) => m + (level -> collectPar))
            } yield Unit
          }
        }
      // Try to find a match for a single pattern.
      case (targets, pattern +: prem) => {
        if (!lf.connectiveUsed(pattern)) {
          possiblyRemove(pattern, targets) match {
            case None => StateT.liftF(Stream.Empty)
            case Some(filtered) =>
              listMatch(filtered, prem, merger, varLevel, wildcard)
          }
        } else {
          for {
            trem        <- listMatchItem(targets, pattern, spatialMatch[T, T])
            forcedYield <- listMatch(trem, prem, merger, varLevel, wildcard)
          } yield forcedYield
        }
      }
    }

  /** TODO(mateusz.gorski): Consider moving this inside [[listMatchItem]]
    */
  private[this] def singleOut[A](vals: Seq[A]): Seq[(Seq[A], A, Seq[A])] =
    vals.tails
      .foldLeft((Seq[A](), Seq[(Seq[A], A, Seq[A])]())) {
        case ((head, singled: Seq[(Seq[A], A, Seq[A])]), Nil) => (head, singled)
        case ((head, singled: Seq[(Seq[A], A, Seq[A])]), elem +: tail) =>
          (elem +: head, (head, elem, tail) +: singled)
      }
      ._2
      .reverse

  private[this] def listMatchItem[T](
      tlist: Seq[T],
      pattern: T,
      matcher: (T, T) => OptionalFreeMap[Unit]): NonDetFreeMap[Seq[T]] =
    for {
      triple               <- StateT.liftF(singleOut(tlist).toStream)
      (head, target, tail) = triple
      forcedYield <- StateT[Stream, FreeMap, Seq[T]]((s: FreeMap) => {
                      matcher(target, pattern).run(s) match {
                        case None =>
                          Stream.Empty
                        case Some((state, _)) =>
                          Stream((state, head.reverse ++ tail))
                      }
                    })
    } yield forcedYield

  implicit val parSpatialMatcherInstance: SpatialMatcher[Par, Par] = fromNonDetFunction[Par, Par] {
    (target, pattern) =>
      if (!pattern.connectiveUsed) {
        if (pattern == target)
          StateT.pure(Unit)
        else {
          StateT.liftF(Stream.Empty)
        }
      } else {
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

        val filteredPattern  = pattern.withExprs(noFrees(pattern.exprs))
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
          : NonDetFreeMap[Par] = {
          val (con, bounds, remainders) = labeledConnective
          for {
            sp <- StateT.liftF(subPars(target, bounds._1, bounds._2, remainders._1, remainders._2))
            _  <- nonDetMatch(sp._1, con)
          } yield sp._2
        }
        for {
          remainder <- Foldable[List].foldM(connectivesWithBounds, target)(
                        matchConnectiveWithBounds)
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
      if (pattern == target)
        StateT.pure(Unit)
      else {
        StateT.liftF(None)
      }
    }

  implicit val sendSpatialMatcherInstance: SpatialMatcher[Send, Send] = fromFunction[Send, Send] {
    (target, pattern) =>
      if (target.persistent != pattern.persistent)
        StateT.liftF(None)
      else
        for {
          _ <- spatialMatch(target.chan, pattern.chan)
          _ <- foldMatch(target.data, pattern.data)
        } yield Unit
  }

  implicit val receiveSpatialMatcherInstance: SpatialMatcher[Receive, Receive] =
    fromFunction[Receive, Receive] { (target, pattern) =>
      if (target.persistent != pattern.persistent)
        StateT.liftF(None)
      else
        for {
          _ <- listMatchSingle[ReceiveBind](target.binds, pattern.binds, (p, rb) => p, None, false)
          _ <- spatialMatch(target.body, pattern.body)
        } yield Unit
    }

  implicit val newSpatialMatcherInstance: SpatialMatcher[New, New] = fromFunction[New, New] {
    (target, pattern) =>
      if (target.bindCount == pattern.bindCount)
        spatialMatch(target.p, pattern.p)
      else
        StateT.liftF(None)
  }

  implicit val exprSpatialMatcherInstance: SpatialMatcher[Expr, Expr] = fromFunction[Expr, Expr] {
    (target, pattern) =>
      (target.exprInstance, pattern.exprInstance) match {
        case (EListBody(EList(tlist, _, _, _)), EListBody(EList(plist, _, _, rem))) => {
          for {
            matchedRem <- foldMatch(tlist, plist, rem)
            _ <- rem match {
                  case Some(Var(FreeVar(level))) =>
                    StateT.modify[Option, FreeMap](m => m + (level -> EList(matchedRem)))
                  case _ => StateT.pure[Option, FreeMap, Unit](Unit)
                }
          } yield Unit
        }
        case (ETupleBody(ETuple(tlist, _, _)), ETupleBody(ETuple(plist, _, _))) => {
          foldMatch(tlist, plist).map(_ => Unit)
        }
        case (EVarBody(EVar(vp)), EVarBody(EVar(vt))) =>
          if (vp == vt) StateT.pure(Unit) else StateT.liftF(None)
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
        case (EPlusBody(EPlus(t1, t2)), EPlusBody(EPlus(p1, p2))) =>
          for {
            _ <- spatialMatch(t1, p1)
            _ <- spatialMatch(t2, p2)
          } yield Unit
        case (EEvalBody(chan1), EEvalBody(chan2)) =>
          spatialMatch(chan1, chan2)
        case _ => StateT.liftF(None)
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
      if (target == pattern)
        StateT.pure(Unit)
      else
        StateT.liftF(None)
    }

  implicit val channelSpatialMatcherInstance: SpatialMatcher[Channel, Channel] =
    fromFunction[Channel, Channel] { (target, pattern) =>
      (target.channelInstance, pattern.channelInstance) match {
        case (_, ChanVar(v)) if v.varInstance.isWildcard => StateT.pure(Unit)
        case (Quote(p), ChanVar(v)) => {
          v.varInstance match {
            case FreeVar(level) => {
              if (p.locallyFree.isEmpty) StateT.modify(m => m + (level -> p))
              else StateT.liftF(None)
            }
            case _ => StateT.liftF(None)
          }
        }
        case (ChanVar(tv), ChanVar(pv)) =>
          (tv.varInstance, pv.varInstance) match {
            case (BoundVar(tlevel), BoundVar(plevel)) => {
              if (tlevel === plevel) StateT.pure(Unit) else StateT.liftF(None)
            }
            case _ => StateT.liftF(None)
          }
        case (Quote(tproc), Quote(pproc)) => spatialMatch(tproc, pproc)
        case _                            => StateT.liftF(None)
      }
    }

  implicit val receiveBindSpatialMatcherInstance: SpatialMatcher[ReceiveBind, ReceiveBind] =
    fromFunction[ReceiveBind, ReceiveBind] { (target, pattern) =>
      if (target.patterns != pattern.patterns)
        StateT.liftF[Option, FreeMap, Unit](None)
      else
        spatialMatch(target.source, pattern.source)
    }

  implicit val matchCaseSpatialMatcherInstance: SpatialMatcher[MatchCase, MatchCase] =
    fromFunction[MatchCase, MatchCase] { (target, pattern) =>
      if (target.pattern != pattern.pattern)
        StateT.liftF(None)
      else
        spatialMatch(target.source, pattern.source)
    }

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
          def firstMatch(target: Par, patterns: Seq[Par]): OptionalFreeMap[Unit] =
            patterns match {
              case Nil => StateT.liftF(None)
              case p +: rem =>
                StateT[Option, FreeMap, Unit]((s: FreeMap) => {
                  spatialMatch(target, p).run(s) match {
                    case None               => firstMatch(target, rem).run(s)
                    case Some((_, _: Unit)) => Some((s, Unit))
                  }
                })
            }
          firstMatch(target, ps)
        }
        case ConnNotBody(p) =>
          StateT[Option, FreeMap, Unit]((s: FreeMap) => {
            spatialMatch(target, p).run(s) match {
              case None         => Some((s, Unit))
              case Some((_, _)) => None
            }
          })
      }
    }
}
