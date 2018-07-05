package coop.rchain.rholang.interpreter

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sort._
import coop.rchain.rholang.interpreter.accounting.CostAccountingAlg
import coop.rchain.rholang.interpreter.errors.SubstituteError

trait Substitute[M[_], A] {
  def substitute(term: A, depth: Int)(implicit env: Env[Par],
                                      costAccounting: CostAccountingAlg[M]): M[A]
  def substituteNoSort(term: A, depth: Int)(implicit env: Env[Par],
                                            costAccounting: CostAccountingAlg[M]): M[A]
}

object Substitute {
  private[this] def substitute2[M[_]: Monad, A, B, C](termA: A, termB: B, depth: Int)(
      f: (A, B) => C)(implicit evA: Substitute[M, A],
                      evB: Substitute[M, B],
                      costAccountingAlg: CostAccountingAlg[M],
                      env: Env[Par]): M[C] =
    (evA.substitute(termA, depth), evB.substitute(termB, depth)).mapN(f)

  private[this] def substituteNoSort2[M[_]: Monad, A, B, C](termA: A, termB: B, depth: Int)(
      f: (A, B) => C)(implicit evA: Substitute[M, A],
                      evB: Substitute[M, B],
                      costAccountingAlg: CostAccountingAlg[M],
                      env: Env[Par]): M[C] =
    (evA.substituteNoSort(termA, depth), evB.substituteNoSort(termB, depth)).mapN(f)

  private[this] def maybeSubstitute[M[_]: Sync](term: Var, depth: Int)(
      implicit costAccountingAlg: CostAccountingAlg[M],
      env: Env[Par]): M[Either[Var, Par]] =
    if (depth != 0)
      costAccountingAlg.charge(term.serializedSize) *>
        Sync[M].pure(Left(term))
    else
      term.varInstance match {
        case BoundVar(index) =>
          env.get(index) match {
            case Some(par) =>
              costAccountingAlg.charge(par.serializedSize) *>
                Sync[M].pure(Right(par))
            case None =>
              costAccountingAlg.charge(term.serializedSize) *>
                Sync[M].pure(Left(term))
          }
        case _ =>
          costAccountingAlg.charge(term.serializedSize) *>
            Sync[M].raiseError(SubstituteError(s"Illegal Substitution [$term]"))
      }

  def maybeSubstitute[M[_]: Sync](term: EVar, depth: Int)(
      implicit costAccountingAlg: CostAccountingAlg[M],
      env: Env[Par]): M[Either[EVar, Par]] =
    maybeSubstitute[M](term.v, depth).map {
      case Left(_)    => Left(term)
      case Right(par) => Right(par)
    }

  def maybeSubstitute[M[_]: Sync](term: EEvalBody, depth: Int)(
      implicit env: Env[Par],
      costAccountingAlg: CostAccountingAlg[M]): M[Either[Expr, Par]] =
    term.value.channelInstance match {
      case Quote(p) => substitutePar[M].substituteNoSort(p, depth).map(Right(_))
      case ChanVar(v) =>
        maybeSubstitute[M](v, depth).map {
          case Left(v)    => Left(Expr(EEvalBody(ChanVar(v))))
          case Right(par) => Right(par)
        }
      case ChannelInstance.Empty => Sync[M].pure(Left(Expr(term)))
    }

  def maybeSubstitute[M[_]: Sync](term: VarRef, depth: Int)(
      implicit env: Env[Par],
      costAccountingAlg: CostAccountingAlg[M]): M[Either[VarRef, Par]] =
    if (term.depth != depth)
      costAccountingAlg.charge(term.serializedSize) *>
        Sync[M].pure(Left(term))
    else
      env.get(term.index) match {
        case Some(par) =>
          costAccountingAlg.charge(par.serializedSize) *>
            Sync[M].pure(Right(par))
        case None =>
          costAccountingAlg.charge(term.serializedSize) *>
            Sync[M].pure(Left(term))
      }

  implicit def substituteQuote[M[_]: Sync]: Substitute[M, Quote] =
    new Substitute[M, Quote] {

      override def substitute(term: Quote, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Quote] =
        substitutePar[M].substitute(term.value, depth).map(subst => Quote(subst))

      override def substituteNoSort(term: Quote, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Quote] =
        substitutePar[M].substituteNoSort(term.value, depth).map(subst => Quote(subst))
    }

  implicit def substituteBundle[M[_]: Sync]: Substitute[M, Bundle] =
    new Substitute[M, Bundle] {
      import BundleOps._

      override def substitute(term: Bundle, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Bundle] =
        substitutePar[M].substitute(term.body, depth).map { subBundle =>
          subBundle.singleBundle() match {
            case Some(value) => term.merge(value)
            case None        => term.copy(body = subBundle)
          }
        }

      override def substituteNoSort(term: Bundle, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Bundle] =
        substitutePar[M].substituteNoSort(term.body, depth).map { subBundle =>
          subBundle.singleBundle() match {
            case Some(value) => term.merge(value)
            case None        => term.copy(body = subBundle)
          }
        }
    }

  implicit def substituteChannel[M[_]: Sync]: Substitute[M, Channel] =
    new Substitute[M, Channel] {

      override def substituteNoSort(term: Channel, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Channel] =
        for {
          channelSubst <- term.channelInstance match {
                           case Quote(p) => substitutePar[M].substitute(p, depth).map(Quote(_))
                           case ChanVar(v) =>
                             maybeSubstitute[M](v, depth).map {
                               case Left(_v) => ChanVar(_v)
                               case Right(p) => Quote(p)
                             }
                           case ChannelInstance.Empty => Sync[M].pure(term.channelInstance)
                         }
        } yield channelSubst

      override def substitute(term: Channel, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Channel] =
        substituteNoSort(term, depth).map(channelSubst =>
          ChannelSortMatcher.sortMatch(channelSubst).term)
    }

  implicit def substitutePar[M[_]: Sync]: Substitute[M, Par] =
    new Substitute[M, Par] {
      def subExp(exprs: Seq[Expr], depth: Int)(implicit
                                               env: Env[Par],
                                               costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        exprs.toList.reverse.foldM(VectorPar()) { (par, expr) =>
          expr.exprInstance match {
            case EVarBody(e) =>
              maybeSubstitute[M](e, depth).map {
                case Left(_e)    => par.prepend(_e)
                case Right(_par) => _par ++ par
              }
            case e: EEvalBody =>
              maybeSubstitute[M](e, depth).map {
                case Left(expr)  => par.prepend(expr)
                case Right(_par) => _par ++ par
              }
            case _ => substituteExpr[M].substituteNoSort(expr, depth).map(par.prepend(_))
          }
        }

      def subConn(conns: Seq[Connective], depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        conns.toList.reverse.foldM(VectorPar()) { (par, conn) =>
          conn.connectiveInstance match {
            case VarRefBody(v) =>
              maybeSubstitute[M](v, depth).map {
                case Left(_)       => par.prepend(conn)
                case Right(newPar) => newPar ++ par
              }
            case ConnectiveInstance.Empty => par.pure[M]
            case ConnAndBody(ConnectiveBody(ps)) =>
              ps.toVector
                .traverse(substitutePar[M].substituteNoSort(_, depth))
                .map(ps => Connective(ConnAndBody(ConnectiveBody(ps))))
            case ConnOrBody(ConnectiveBody(ps)) =>
              ps.toVector
                .traverse(substitutePar[M].substituteNoSort(_, depth))
                .map(ps => Connective(ConnOrBody(ConnectiveBody(ps))))
            case ConnNotBody(p) =>
              substitutePar[M].substituteNoSort(p, depth).map(p => Connective(ConnNotBody(p)))
          }
        }

      override def substituteNoSort(term: Par, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        for {
          exprs       <- subExp(term.exprs, depth)
          connectives <- subConn(term.connectives, depth)
          sends       <- term.sends.toVector.traverse(substituteSend[M].substituteNoSort(_, depth))
          bundles     <- term.bundles.toVector.traverse(substituteBundle[M].substituteNoSort(_, depth))
          receives <- term.receives.toVector
                       .traverse(substituteReceive[M].substituteNoSort(_, depth))
          news    <- term.news.toVector.traverse(substituteNew[M].substituteNoSort(_, depth))
          matches <- term.matches.toVector.traverse(substituteMatch[M].substituteNoSort(_, depth))
          par = exprs ++
            connectives ++
            Par(
              exprs = Nil,
              sends = sends,
              bundles = bundles,
              receives = receives,
              news = news,
              matches = matches,
              ids = term.ids,
              connectives = Nil,
              locallyFree = term.locallyFree.until(env.shift),
              connectiveUsed = term.connectiveUsed
            )
        } yield par

      override def substitute(term: Par, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Par] =
        substituteNoSort(term, depth).map(par => ParSortMatcher.sortMatch(par).term)
    }

  implicit def substituteSend[M[_]: Sync]: Substitute[M, Send] =
    new Substitute[M, Send] {
      override def substituteNoSort(term: Send, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Send] =
        for {
          channelsSub <- substituteChannel[M].substituteNoSort(term.chan, depth)
          parsSub     <- term.data.toVector.traverse(substitutePar[M].substituteNoSort(_, depth))
          send = Send(
            chan = channelsSub,
            data = parsSub,
            persistent = term.persistent,
            locallyFree = term.locallyFree.until(env.shift),
            connectiveUsed = term.connectiveUsed
          )
        } yield send

      override def substitute(term: Send, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Send] =
        substituteNoSort(term, depth).map(send => SendSortMatcher.sortMatch(send).term)
    }

  implicit def substituteReceive[M[_]: Sync]: Substitute[M, Receive] =
    new Substitute[M, Receive] {
      override def substituteNoSort(term: Receive, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Receive] =
        for {
          bindsSub <- term.binds.toVector.traverse {
                       case ReceiveBind(patterns, chan, rem, freeCount) =>
                         for {
                           subChannel <- substituteChannel[M].substituteNoSort(chan, depth)
                           subPatterns <- patterns.toVector.traverse(
                                           pattern =>
                                             substituteChannel[M]
                                               .substituteNoSort(pattern, depth + 1)(
                                                 env,
                                                 costAccountingAlg))
                         } yield ReceiveBind(subPatterns, subChannel, rem, freeCount)
                     }
          bodySub <- substitutePar[M].substituteNoSort(term.body, depth)(env.shift(term.bindCount),
                                                                         costAccountingAlg)
          rec = Receive(
            binds = bindsSub,
            body = bodySub,
            persistent = term.persistent,
            bindCount = term.bindCount,
            locallyFree = term.locallyFree.until(env.shift),
            connectiveUsed = term.connectiveUsed
          )
        } yield rec

      override def substitute(term: Receive, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Receive] =
        substituteNoSort(term, depth).map(rec => ReceiveSortMatcher.sortMatch(rec).term)

    }

  implicit def substituteNew[M[_]: Sync]: Substitute[M, New] =
    new Substitute[M, New] {
      override def substituteNoSort(term: New, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[New] =
        for {
          newSub <- substitutePar[M]
                     .substituteNoSort(term.p, depth)(env.shift(term.bindCount), costAccountingAlg)
          neu = New(term.bindCount, newSub, term.locallyFree.until(env.shift))
        } yield neu
      override def substitute(term: New, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[New] =
        substituteNoSort(term, depth).map(newSub => NewSortMatcher.sortMatch(newSub).term)
    }

  implicit def substituteMatch[M[_]: Sync]: Substitute[M, Match] =
    new Substitute[M, Match] {
      override def substituteNoSort(term: Match, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Match] =
        for {
          targetSub <- substitutePar[M].substituteNoSort(term.target, depth)
          casesSub <- term.cases.toVector.traverse {
                       case MatchCase(_case, _par, freeCount) =>
                         for {
                           par <- substitutePar[M].substituteNoSort(_par, depth)(
                                   env.shift(freeCount),
                                   costAccountingAlg)
                           subCase <- substitutePar[M].substituteNoSort(_case, depth + 1)
                         } yield MatchCase(subCase, par, freeCount)
                     }
          mat = Match(targetSub, casesSub, term.locallyFree.until(env.shift), term.connectiveUsed)
        } yield mat

      override def substitute(term: Match, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Match] =
        substituteNoSort(term, depth).map(mat => MatchSortMatcher.sortMatch(mat).term)
    }

  implicit def substituteExpr[M[_]: Sync]: Substitute[M, Expr] =
    new Substitute[M, Expr] {
      private[this] def substituteDelegate(
          term: Expr,
          s1: Par => M[Par],
          s2: (Par, Par) => ((Par, Par) => Expr) => M[Expr])(implicit env: Env[Par]): M[Expr] =
        term.exprInstance match {
          case ENotBody(ENot(par)) => s1(par).map(ENot(_))
          case ENegBody(ENeg(par)) => s1(par).map(ENeg(_))
          case EMultBody(EMult(par1, par2)) =>
            s2(par1, par2)(EMult(_, _))
          case EDivBody(EDiv(par1, par2)) =>
            s2(par1, par2)(EDiv(_, _))
          case EPlusBody(EPlus(par1, par2)) =>
            s2(par1, par2)(EPlus(_, _))
          case EMinusBody(EMinus(par1, par2)) =>
            s2(par1, par2)(EMinus(_, _))
          case ELtBody(ELt(par1, par2)) =>
            s2(par1, par2)(ELt(_, _))
          case ELteBody(ELte(par1, par2)) =>
            s2(par1, par2)(ELte(_, _))
          case EGtBody(EGt(par1, par2)) =>
            s2(par1, par2)(EGt(_, _))
          case EGteBody(EGte(par1, par2)) =>
            s2(par1, par2)(EGte(_, _))
          case EEqBody(EEq(par1, par2)) =>
            s2(par1, par2)(EEq(_, _))
          case ENeqBody(ENeq(par1, par2)) =>
            s2(par1, par2)(ENeq(_, _))
          case EAndBody(EAnd(par1, par2)) =>
            s2(par1, par2)(EAnd(_, _))
          case EOrBody(EOr(par1, par2)) =>
            s2(par1, par2)(EOr(_, _))
          case EListBody(EList(ps, locallyFree, connectiveUsed, rem)) =>
            for {
              pss <- ps.toVector
                      .traverse(p => s1(p))
              newLocallyFree = locallyFree.until(env.shift)
            } yield Expr(exprInstance = EListBody(EList(pss, newLocallyFree, connectiveUsed, rem)))

          case ETupleBody(ETuple(ps, locallyFree, connectiveUsed)) =>
            for {
              pss <- ps.toVector
                      .traverse(p => s1(p))
              newLocallyFree = locallyFree.until(env.shift)
            } yield Expr(exprInstance = ETupleBody(ETuple(pss, newLocallyFree, connectiveUsed)))

          case ESetBody(ParSet(shs, connectiveUsed, locallyFree)) =>
            for {
              pss <- shs.sortedPars
                      .traverse(p => s1(p))
            } yield
              Expr(exprInstance = ESetBody(
                ParSet(SortedParHashSet(pss), connectiveUsed, locallyFree.map(_.until(env.shift)))))

          case EMapBody(ParMap(spm, connectiveUsed, locallyFree)) =>
            for {
              kvps <- spm.sortedMap.traverse {
                       case (p1, p2) =>
                         for {
                           pk1 <- s1(p1)
                           pk2 <- s1(p2)
                         } yield (pk1, pk2)
                     }
            } yield
              Expr(
                exprInstance =
                  EMapBody(ParMap(kvps, connectiveUsed, locallyFree.map(_.until(env.shift)))))
          case EMethodBody(EMethod(mtd, target, arguments, locallyFree, connectiveUsed)) =>
            for {
              subTarget    <- s1(target)
              subArguments <- arguments.toVector.traverse(p => s1(p))
            } yield
              Expr(
                exprInstance = EMethodBody(
                  EMethod(mtd,
                          subTarget,
                          subArguments,
                          locallyFree.until(env.shift),
                          connectiveUsed)))
          case g @ _ => Sync[M].pure(term)
        }
      override def substitute(term: Expr, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Expr] =
        substituteDelegate(term,
                           substitutePar[M].substitute(_, depth),
                           substitute2[M, Par, Par, Expr](_, _, depth))
      override def substituteNoSort(term: Expr, depth: Int)(
          implicit
          env: Env[Par],
          costAccountingAlg: CostAccountingAlg[M]): M[Expr] =
        substituteDelegate(term,
                           substitutePar[M].substituteNoSort(_, depth),
                           substituteNoSort2[M, Par, Par, Expr](_, _, depth))
    }
}
