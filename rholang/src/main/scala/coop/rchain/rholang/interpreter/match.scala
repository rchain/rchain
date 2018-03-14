package coop.rchain.rholang.interpreter

import cats._
import cats.data._
import cats.implicits._

import coop.rchain.models._
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._

import scala.annotation.tailrec
import scala.collection.immutable.Stream

import implicits.{ExprLocallyFree, SendLocallyFree}

// The spatial matcher takes targets and patterns. It uses StateT[Option,
// FreeMap, Unit] to represent the computation. The state is the mapping from
// free variables to the values that were captured. StateT[Option, S, A] allows
// for failure, and when a failure occurs, no new state is provided. This is
// what we want, because when no matching occurs, there is no result map.
// In a few places we use StateT[Stream, FreeMap, A] for backtracking. In order
// to help cut down on backtracking, wherever one of several possible matches
// will do, we just take one.
object SpatialMatcher {
  type FreeMap            = Map[Int, Par]
  type OptionalFreeMap[A] = StateT[Option, FreeMap, A]
  type NonDetFreeMap[A]   = StateT[Stream, FreeMap, A]

  def possiblyRemove[T](needle: T, haystack: Seq[T]): Option[Seq[T]] = {
    val (before, after) = haystack.span(x => x != needle)
    after match {
      case Nil       => None
      case _ +: tail => Some(before ++ tail)
    }
  }

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

  @tailrec
  def exists[T](prop: (T => Boolean), haystack: Seq[T]): Boolean =
    haystack match {
      case Nil => false
      case head :: rest =>
        if (prop(head))
          true
        else
          exists(prop, rest)
    }

  def emptyMap: FreeMap = Map.empty[Int, Par]

  def unLift[F[_], S, A](raw: StateT[F, S, A])(implicit F: Monad[F]): StateT[F, S, F[(S, A)]] =
    StateT(s => F.pure((s, raw.run(s))))

  def singleOut[A](vals: Seq[A]): Seq[(Seq[A], A, Seq[A])] =
    vals.tails
      .foldLeft((Seq[A](), Seq[(Seq[A], A, Seq[A])]())) {
        case ((head, singled: Seq[(Seq[A], A, Seq[A])]), Nil) => (head, singled)
        case ((head, singled: Seq[(Seq[A], A, Seq[A])]), elem +: tail) =>
          (elem +: head, (head, elem, tail) +: singled)
      }
      ._2
      .reverse

  // This helper function is useful in several productions
  def foldMatch(tlist: Seq[Par], plist: Seq[Par]): OptionalFreeMap[Unit] =
    (tlist, plist) match {
      case (Nil, Nil) => StateT.pure(Unit)
      case (Nil, _)   => StateT.liftF(None)
      case (_, Nil)   => StateT.liftF(None)
      case (t +: trem, p +: prem) => {
        spatialMatch(t, p).flatMap(_ => foldMatch(trem, prem))
      }
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
  def listMatchSingle[T](
      tlist: Seq[T],
      plist: Seq[T],
      matcher: (T, T) => OptionalFreeMap[Unit],
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
      listMatch(tlist, plist, matcher, merger, varLevel, wildcard).run(s) match {
        case Stream.Empty =>
          None
        case head #:: _ =>
          Some(head)
      }
    })
  }

  def listMatch[T](tlist: Seq[T],
                   plist: Seq[T],
                   matcher: (T, T) => OptionalFreeMap[Unit],
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
              p          <- StateT.inspect[Stream, FreeMap, Par]((m: FreeMap) => m.getOrElse(level, Par()))
              collectPar <- foldRemainder(rem, p)
              _          <- StateT.modify[Stream, FreeMap]((m: FreeMap) => m + (level -> collectPar))
            } yield Unit
          }
        }
      // Try to find a match for a single pattern.
      case (targets, pattern +: prem) => {
        if (lf.freeCount(pattern) === 0) {
          possiblyRemove(pattern, targets) match {
            case None           => StateT.liftF(Stream.Empty)
            case Some(filtered) => listMatch(filtered, prem, matcher, merger, varLevel, wildcard)
          }
        } else {
          for {
            trem        <- listMatchItem(targets, pattern, matcher)
            forcedYield <- listMatch(trem, prem, matcher, merger, varLevel, wildcard)
          } yield forcedYield
        }
      }
    }

  def listMatchItem[T](tlist: Seq[T],
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

  def spatialMatch(target: Par, pattern: Par): OptionalFreeMap[Unit] =
    if (pattern.freeCount === 0) {
      if (pattern == target)
        StateT.pure(Unit)
      else {
        StateT.liftF(None)
      }
    } else {
      val varLevel: Option[Int] = possiblyFind[Expr, Int](
        { (expr) =>
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
      val wildcard: Boolean = exists[Expr](
        { (expr) =>
          expr.exprInstance match {
            case EVarBody(EVar(Some(v))) =>
              v.varInstance match {
                case Wildcard(_) => true
                case _           => false
              }
            case _ => false
          }
        },
        pattern.exprs
      )

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
                                   spatialMatch,
                                   (p, s) => p.withSends(s +: p.sends),
                                   varLevel,
                                   wildcard)
        _ <- listMatchSingle[Expr](target.exprs,
                                   NoFrees(pattern.exprs),
                                   spatialMatch,
                                   (p, e) => p.withExprs(e +: p.exprs),
                                   varLevel,
                                   wildcard)
      } yield Unit
    }

  def spatialMatch(target: Channel, pattern: Channel): OptionalFreeMap[Unit] =
    (target.channelInstance, pattern.channelInstance) match {
      case (_, ChanVar(v)) if v.varInstance.isWildcard => StateT.pure(Unit)
      case (Quote(p), ChanVar(v)) => {
        v.varInstance match {
          case FreeVar(level) => {
            if (p.locallyFree.isEmpty)
              StateT.modify(m => m + (level -> p))
            else
              StateT.liftF(None)
          }
          case _ => StateT.liftF(None)
        }
      }
      case (ChanVar(tv), ChanVar(pv)) =>
        (tv.varInstance, pv.varInstance) match {
          case (BoundVar(tlevel), BoundVar(plevel)) => {
            if (tlevel === plevel)
              StateT.pure(Unit)
            else
              StateT.liftF(None)
          }
          case _ => StateT.liftF(None)
        }
      case (Quote(tproc), Quote(pproc)) => spatialMatch(tproc, pproc)
      case _                            => StateT.liftF(None)
    }

  def spatialMatch(target: Send, pattern: Send): OptionalFreeMap[Unit] =
    if (target.persistent != pattern.persistent)
      StateT.liftF(None)
    else
      for {
        _           <- spatialMatch(target.chan.get, pattern.chan.get)
        forcedYield <- foldMatch(target.data, pattern.data)
      } yield forcedYield

  def spatialMatch(target: Expr, pattern: Expr): OptionalFreeMap[Unit] =
    (target.exprInstance, pattern.exprInstance) match {
      case (EListBody(EList(tlist, _, _)), EListBody(EList(plist, _, _))) => {
        foldMatch(tlist, plist)
      }
      case (ETupleBody(ETuple(tlist, _, _)), ETupleBody(ETuple(plist, _, _))) => {
        foldMatch(tlist, plist)
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
