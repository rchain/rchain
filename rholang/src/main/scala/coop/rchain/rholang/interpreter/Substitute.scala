package coop.rchain.rholang.interpreter

import cats.implicits._
import cats.{Applicative, Monad}
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.errors.{InterpreterErrorsM, SubstituteError}
import coop.rchain.rholang.interpreter.implicits._
import errors._

trait Substitute[M[_], A] {
  def substitute(term: A)(implicit env: Env[Par]): M[A]
}

object Substitute {

  def substitute2[M[_]: Monad, A, B, C](termA: A, termB: B)(
      f: (A, B) => C)(implicit evA: Substitute[M, A], evB: Substitute[M, B], env: Env[Par]): M[C] =
    for {
      aSub <- evA.substitute(termA)
      bSub <- evB.substitute(termB)
    } yield f(aSub, bSub)

  def apply[M[_], A](implicit ev: Substitute[M, A]): Substitute[M, A] = ev

  def maybeSubstitute[M[+ _]: InterpreterErrorsM](term: Var)(
      implicit env: Env[Par]): M[Either[Var, Par]] =
    term.varInstance match {
      case BoundVar(index) =>
        env.get(index) match {
          case Some(par) => Applicative[M].pure(Right(par))
          case None =>
            Applicative[M].pure(Left[Var, Par](BoundVar(index))) //scalac is not helping here
        }
      case _ =>
        interpreterErrorM[M].raiseError(SubstituteError(s"Illegal Substitution [$term]"))
    }

  def maybeSubstitute[M[_]: InterpreterErrorsM](term: EVar)(
      implicit env: Env[Par]): M[Either[EVar, Par]] =
    maybeSubstitute[M](term.v.get).map {
      case Left(v)    => Left(EVar(v))
      case Right(par) => Right(par)
    }

  def maybeSubstitute[M[_]: InterpreterErrorsM](term: Eval)(
      implicit env: Env[Par]): M[Either[Eval, Par]] =
    term.channel.get.channelInstance match {
      case Quote(p) => substitutePar[M].substitute(p).map(Right(_))
      case ChanVar(v) =>
        maybeSubstitute[M](v).map {
          case Left(v)    => Left(Eval(ChanVar(v)))
          case Right(par) => Right(par)
        }
    }

  implicit def substituteQuote[M[_]: InterpreterErrorsM]: Substitute[M, Quote] =
    new Substitute[M, Quote] {
      override def substitute(term: Quote)(implicit env: Env[Par]): M[Quote] =
        substitutePar[M].substitute(term.value).map(Quote(_))
    }

  implicit def substituteBundle[M[_]: InterpreterErrorsM]: Substitute[M, Bundle] =
    new Substitute[M, Bundle] {
      import BundleOps._

      override def substitute(term: Bundle)(implicit env: Env[Par]): M[Bundle] =
        substitutePar[M].substitute(term.body.get).map { subBundle =>
          subBundle.singleBundle() match {
            case Some(value) => term.merge(value)
            case None        => term.copy(body = subBundle)
          }
        }
    }

  implicit def substituteChannel[M[_]: InterpreterErrorsM]: Substitute[M, Channel] =
    new Substitute[M, Channel] {
      override def substitute(term: Channel)(implicit env: Env[Par]): M[Channel] =
        for {
          channelSubst <- term.channelInstance match {
                           case Quote(p) => substitutePar[M].substitute(p).map(Quote(_))
                           case ChanVar(v) =>
                             maybeSubstitute[M](v).map {
                               case Left(_v) => ChanVar(_v)
                               case Right(p) => Quote(p)
                             }
                         }
          sortedChan <- ChannelSortMatcher.sortMatch[M](channelSubst)
        } yield sortedChan.term
    }

  implicit def substitutePar[M[_]: InterpreterErrorsM]: Substitute[M, Par] =
    new Substitute[M, Par] {
      def subExp(exprs: Seq[Expr])(implicit env: Env[Par]): M[Par] =
        exprs.toList.reverse.foldM(VectorPar()) { (par, expr) =>
          expr.exprInstance match {
            case EVarBody(e @ EVar(_)) =>
              maybeSubstitute[M](e).map {
                case Left(_e)    => par.prepend(_e)
                case Right(_par) => _par ++ par
              }
            case _ => substituteExpr[M].substitute(expr).map(par.prepend(_))
          }
        }

      def subEval(evals: Seq[Eval])(implicit env: Env[Par]): M[Par] =
        evals.toList.reverse.foldM(VectorPar()) { (par, eval) =>
          maybeSubstitute[M](eval).map {
            case Left(plainEval)   => par.prepend(plainEval)
            case Right(droppedPar) => droppedPar ++ par
          }
        }

      override def substitute(term: Par)(implicit env: Env[Par]): M[Par] =
        for {
          exprs    <- subExp(term.exprs)
          evals    <- subEval(term.evals)
          sends    <- term.sends.toList.traverse(substituteSend[M].substitute(_))
          bundles  <- term.bundles.toList.traverse(substituteBundle[M].substitute(_))
          receives <- term.receives.toList.traverse(substituteReceive[M].substitute(_))
          news     <- term.news.toList.traverse(substituteNew[M].substitute(_))
          matches  <- term.matches.toList.traverse(substituteMatch[M].substitute(_))
          par = exprs ++
            evals ++
            Par(
              exprs = Nil,
              evals = Nil,
              sends = sends,
              bundles = bundles,
              receives = receives,
              news = news,
              matches = matches,
              ids = term.ids,
              locallyFree = term.locallyFree.until(env.shift),
              connectiveUsed = term.connectiveUsed
            )
          // This may be a minor thing, but sorting this par causes all of the insides to be sorted.
          // Maybe we want to split substitute so that there's an external API that sorts and an internal API that doesn't
          sortedPar <- ParSortMatcher.sortMatch[M](par)
        } yield sortedPar.term.get

    }

  implicit def substituteSend[M[_]: InterpreterErrorsM]: Substitute[M, Send] =
    new Substitute[M, Send] {
      override def substitute(term: Send)(implicit env: Env[Par]): M[Send] =
        for {
          channelsSub <- substituteChannel[M].substitute(term.chan.get)
          parsSub     <- term.data.toList.traverse(substitutePar[M].substitute(_))
          send = Send(
            chan = channelsSub,
            data = parsSub,
            persistent = term.persistent,
            locallyFree = term.locallyFree.until(env.shift),
            connectiveUsed = term.connectiveUsed
          )
          sortedSend <- SendSortMatcher.sortMatch[M](send)
        } yield sortedSend.term
    }

  implicit def substituteReceive[M[_]: InterpreterErrorsM]: Substitute[M, Receive] =
    new Substitute[M, Receive] {
      override def substitute(term: Receive)(implicit env: Env[Par]): M[Receive] =
        for {
          bindsSub <- term.binds.toList.traverse {
                       case ReceiveBind(xs, Some(chan), rem, freeCount) =>
                         substituteChannel[M].substitute(chan).map { (subChannel: Channel) =>
                           ReceiveBind(xs, subChannel, rem, freeCount)
                         }
                     }
          bodySub <- substitutePar[M].substitute(term.body.get)(env.shift(term.bindCount))
          rec = Receive(
            binds = bindsSub,
            body = bodySub,
            persistent = term.persistent,
            bindCount = term.bindCount,
            locallyFree = term.locallyFree.until(env.shift),
            connectiveUsed = term.connectiveUsed
          )
          sortedReceive <- ReceiveSortMatcher.sortMatch[M](rec)
        } yield sortedReceive.term

    }

  implicit def substituteNew[M[_]: InterpreterErrorsM]: Substitute[M, New] =
    new Substitute[M, New] {
      override def substitute(term: New)(implicit env: Env[Par]): M[New] =
        for {
          newSub <- substitutePar[M].substitute(term.p.get)(env.shift(term.bindCount))
          sortedMatch <- NewSortMatcher
                          .sortMatch[M](
                            New(term.bindCount, newSub, term.locallyFree.until(env.shift)))
        } yield sortedMatch.term
    }

  implicit def substituteMatch[M[_]: InterpreterErrorsM]: Substitute[M, Match] =
    new Substitute[M, Match] {
      override def substitute(term: Match)(implicit env: Env[Par]): M[Match] =
        for {
          targetSub <- substitutePar[M].substitute(term.target.get)
          casesSub <- term.cases.toList.traverse {
                       case MatchCase(_case, Some(_par), freeCount) =>
                         substitutePar[M]
                           .substitute(_par)(env.shift(freeCount))
                           .map(par => MatchCase(_case, par, freeCount))
                     }
          sortedMatch <- MatchSortMatcher
                          .sortMatch[M](
                            Match(targetSub,
                                  casesSub,
                                  term.locallyFree.until(env.shift),
                                  term.connectiveUsed))
        } yield sortedMatch.term
    }

  implicit def substituteExpr[M[_]: InterpreterErrorsM]: Substitute[M, Expr] =
    new Substitute[M, Expr] {
      override def substitute(term: Expr)(implicit env: Env[Par]): M[Expr] =
        term.exprInstance match {
          case ENotBody(ENot(par)) => substitutePar[M].substitute(par.get).map(ENot(_))
          case ENegBody(ENeg(par)) => substitutePar[M].substitute(par.get).map(ENeg(_))
          case EMultBody(EMult(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EMult(_, _))
          case EDivBody(EDiv(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EDiv(_, _))
          case EPlusBody(EPlus(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EPlus(_, _))
          case EMinusBody(EMinus(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EMinus(_, _))
          case ELtBody(ELt(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(ELt(_, _))
          case ELteBody(ELte(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(ELte(_, _))
          case EGtBody(EGt(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EGt(_, _))
          case EGteBody(EGte(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EGte(_, _))
          case EEqBody(EEq(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EEq(_, _))
          case ENeqBody(ENeq(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(ENeq(_, _))
          case EAndBody(EAnd(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EAnd(_, _))
          case EOrBody(EOr(par1, par2)) =>
            substitute2[M, Par, Par, Expr](par1.get, par2.get)(EOr(_, _))
          case EListBody(EList(ps, locallyFree, connectiveUsed, rem)) =>
            for {
              pss <- ps.toList
                      .traverse(p => substitutePar[M].substitute(p))
              newLocallyFree = locallyFree.until(env.shift)
            } yield Expr(exprInstance = EListBody(EList(pss, newLocallyFree, connectiveUsed, rem)))

          case ETupleBody(ETuple(ps, locallyFree, connectiveUsed)) =>
            for {
              pss <- ps.toList
                      .traverse(p => substitutePar[M].substitute(p))
              newLocallyFree = locallyFree.until(env.shift)
            } yield Expr(exprInstance = ETupleBody(ETuple(pss, newLocallyFree, connectiveUsed)))

          case ESetBody(ESet(ps, locallyFree, connectiveUsed)) =>
            for {
              pss <- ps.toList
                      .traverse(p => substitutePar[M].substitute(p))
              newLocallyFree = locallyFree.until(env.shift)
            } yield Expr(exprInstance = ESetBody(ESet(pss, newLocallyFree, connectiveUsed)))

          case EMapBody(EMap(kvs, locallyFree, connectiveUsed)) =>
            for {
              kvps <- kvs.toList
                       .traverse {
                         case KeyValuePair(p1, p2) =>
                           for {
                             pk1 <- substitutePar[M].substitute(p1.get)
                             pk2 <- substitutePar[M].substitute(p2.get)
                           } yield KeyValuePair(pk1, pk2)
                       }
              newLocallyFree = locallyFree.until(env.shift)
            } yield Expr(exprInstance = EMapBody(EMap(kvps, newLocallyFree, connectiveUsed)))
          case g @ _ => Applicative[M].pure(term)
        }
    }
}
