package coop.rchain.rholang.interpreter

import cats.data._
import cats.implicits._
import cats.{Eval => _}
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.SpatialMatcher.OptionalFreeMap
import coop.rchain.rholang.interpreter.implicits.{
  fromEList,
  fromExpr,
  BundleLocallyFree,
  ExprLocallyFree,
  GPrivateLocallyFree,
  MatchCaseLocallyFree,
  ParLocallyFree,
  ReceiveBindLocallyFree,
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
trait SpatialMatcher[T] {
  def spatialMatch(target: T, pattern: T): OptionalFreeMap[Unit]
}

object SpatialMatcher {
  def apply[T](implicit sm: SpatialMatcher[T]) = sm

  def spatialMatch[T: SpatialMatcher](target: T, pattern: T): OptionalFreeMap[Unit] =
    SpatialMatcher[T].spatialMatch(target, pattern)

  def fromFunction[T](fn: (T, T) => OptionalFreeMap[Unit]): SpatialMatcher[T] =
    new SpatialMatcher[T] {
      override def spatialMatch(target: T, pattern: T): OptionalFreeMap[Unit] =
        fn(target, pattern)
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

  // This helper function is useful in several productions
  def foldMatch[T: SpatialMatcher](tlist: Seq[T], plist: Seq[T], remainder: Option[Var] = None)(
      implicit lf: HasLocallyFree[T]): OptionalFreeMap[Seq[T]] =
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
                  if (lf.locallyFree(item).isEmpty)
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
  private[this] def listMatchSingle[T: SpatialMatcher](
      tlist: Seq[T],
      plist: Seq[T],
      merger: (Par, T) => Par,
      varLevel: Option[Int],
      wildcard: Boolean)(implicit lf: HasLocallyFree[T]): OptionalFreeMap[Unit] = {
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
        case Stream.Empty =>
          None
        case head #:: _ =>
          Some(head)
      }
    })
  }

  private[this] def possiblyRemove[T](needle: T, haystack: Seq[T]): Option[Seq[T]] = {
    val (before, after) = haystack.span(x => x != needle)
    after match {
      case Nil       => None
      case _ +: tail => Some(before ++ tail)
    }
  }

  private[this] def listMatch[T: SpatialMatcher](
      tlist: Seq[T],
      plist: Seq[T],
      merger: (Par, T) => Par,
      varLevel: Option[Int],
      wildcard: Boolean)(implicit lf: HasLocallyFree[T]): NonDetFreeMap[Unit] =
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
            trem        <- listMatchItem(targets, pattern, spatialMatch[T])
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

  implicit val parSpatialMatcherInstance: SpatialMatcher[Par] = fromFunction[Par] {
    (target, pattern) =>
      if (!pattern.connectiveUsed) {
        if (pattern == target)
          StateT.pure(Unit)
        else {
          StateT.liftF(None)
        }
      } else {
        val varLevel: Option[Int] = possiblyFind[Expr, Int](
          {
            case expr =>
              expr.exprInstance match {
                case EVarBody(EVar(Some(v))) =>
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
            case EVarBody(EVar(Some(v))) =>
              v.varInstance match {
                case Wildcard(_) => true
                case _           => false
              }
            case _ => false
          }
        }

        def NoFrees(exprs: Seq[Expr]): Seq[Expr] =
          exprs.filter({ (expr) =>
            expr.exprInstance match {
              case EVarBody(EVar(Some((v)))) =>
                v.varInstance match {
                  case FreeVar(_)  => false
                  case Wildcard(_) => false
                  case _           => true
                }
              case _ => true
            }
          })

        for {
          _ <- listMatchSingle[Send](target.sends,
                                     pattern.sends,
                                     (p, s) => p.withSends(s +: p.sends),
                                     varLevel,
                                     wildcard)
          _ <- listMatchSingle[Expr](target.exprs,
                                     NoFrees(pattern.exprs),
                                     (p, e) => p.withExprs(e +: p.exprs),
                                     varLevel,
                                     wildcard)
          _ <- listMatchSingle[GPrivate](target.ids,
                                         pattern.ids,
                                         (p, i) => p.withIds(i +: p.ids),
                                         varLevel,
                                         wildcard)
          _ <- listMatchSingle[Bundle](target.bundles,
                                       pattern.bundles,
                                       (p, b) => p.withBundles(b +: p.bundles),
                                       varLevel,
                                       wildcard)
        } yield Unit
      }
  }

  implicit val bundleSpatialMatcherInstance: SpatialMatcher[Bundle] = fromFunction[Bundle] {
    (target, pattern) =>
      if (pattern == target)
        StateT.pure(Unit)
      else {
        StateT.liftF(None)
      }
  }

  implicit val sendSpatialMatcherInstance: SpatialMatcher[Send] = fromFunction[Send] {
    (target, pattern) =>
      if (target.persistent != pattern.persistent)
        StateT.liftF(None)
      else
        for {
          _           <- spatialMatch(target.chan.get, pattern.chan.get)
          forcedYield <- foldMatch(target.data, pattern.data)
        } yield forcedYield
  }

  implicit val receiveSpatialMatcherInstance: SpatialMatcher[Receive] = fromFunction[Receive] {
    (target, pattern) =>
      if (target.persistent != pattern.persistent)
        StateT.liftF(None)
      else
        for {
          _ <- listMatchSingle[ReceiveBind](target.binds, pattern.binds, (p, rb) => p, None, false)
          _ <- spatialMatch(target.body.get, pattern.body.get)
        } yield Unit
  }

  implicit val evalSpatialMatcherInstance: SpatialMatcher[Eval] = fromFunction[Eval] {
    (target, pattern) =>
      spatialMatch(target.channel.get, pattern.channel.get)
  }

  implicit val newSpatialMatcherInstance: SpatialMatcher[New] = fromFunction[New] {
    (target, pattern) =>
      if (target.bindCount == pattern.bindCount)
        spatialMatch(target.p.get, pattern.p.get)
      else
        StateT.liftF(None)
  }

  implicit val exprSpatialMatcherInstance: SpatialMatcher[Expr] = fromFunction[Expr] {
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
        case (ENotBody(ENot(t)), ENotBody(ENot(p))) => spatialMatch(t.get, p.get)
        case (ENegBody(ENeg(t)), ENegBody(ENeg(p))) => spatialMatch(t.get, p.get)
        case (EMultBody(EMult(t1, t2)), EMultBody(EMult(p1, p2))) =>
          for {
            _ <- spatialMatch(t1.get, p1.get)
            _ <- spatialMatch(t2.get, p2.get)
          } yield Unit
        case (EDivBody(EDiv(t1, t2)), EDivBody(EDiv(p1, p2))) =>
          for {
            _ <- spatialMatch(t1.get, p1.get)
            _ <- spatialMatch(t2.get, p2.get)
          } yield Unit
        case (EPlusBody(EPlus(t1, t2)), EPlusBody(EPlus(p1, p2))) =>
          for {
            _ <- spatialMatch(t1.get, p1.get)
            _ <- spatialMatch(t2.get, p2.get)
          } yield Unit
        case _ => StateT.liftF(None)
      }
  }

  implicit val matchSpatialMatcherInstance: SpatialMatcher[Match] = fromFunction[Match] {
    (target, pattern) =>
      for {
        _ <- spatialMatch(target.target.get, pattern.target.get)
        _ <- foldMatch(target.cases, pattern.cases)
      } yield Unit
  }

  /**
    * Note that currently there should be no way to put a GPrivate in a pattern
    * because patterns start with an empty environment.
    * We're going to write the obvious definition anyway.
    */
  implicit val gprivateSpatialMatcherInstance: SpatialMatcher[GPrivate] = fromFunction[GPrivate] {
    (target, pattern) =>
      if (target == pattern)
        StateT.pure(Unit)
      else
        StateT.liftF(None)
  }

  implicit val channelSpatialMatcherInstance: SpatialMatcher[Channel] = fromFunction[Channel] {
    (target, pattern) =>
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

  implicit val receiveBindSpatialMatcherInstance: SpatialMatcher[ReceiveBind] =
    fromFunction[ReceiveBind] { (target, pattern) =>
      if (target.patterns != pattern.patterns)
        StateT.liftF[Option, FreeMap, Unit](None)
      else
        spatialMatch(target.source.get, pattern.source.get)
    }

  implicit val matchCaseSpatialMatcherInstance: SpatialMatcher[MatchCase] =
    fromFunction[MatchCase] { (target, pattern) =>
      if (target.pattern != pattern.pattern)
        StateT.liftF(None)
      else
        spatialMatch(target.source.get, pattern.source.get)
    }

}
