package coop.rchain.rholang.interpreter

import cats.{Applicative, Functor, Monad, MonadError}
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{
  Bundle => AbsynBundle,
  Ground => AbsynGround,
  KeyValuePair => AbsynKeyValuePair,
  Send => AbsynSend,
  _
}
import cats.implicits._
import coop.rchain.rholang.interpreter.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.errors._
import implicits._

import scala.collection.immutable.{BitSet, Vector}

sealed trait VarSort
case object ProcSort extends VarSort
case object NameSort extends VarSort

object BoolNormalizeMatcher {
  def normalizeMatch(b: Bool): GBool =
    b match {
      case _: BoolTrue  => GBool(true)
      case _: BoolFalse => GBool(false)
    }
}

object GroundNormalizeMatcher {
  def normalizeMatch(g: AbsynGround): Expr =
    g match {
      case gb: GroundBool   => BoolNormalizeMatcher.normalizeMatch(gb.bool_)
      case gi: GroundInt    => GInt(gi.integer_)
      case gs: GroundString => GString(gs.string_)
      case gu: GroundUri    => GUri(gu.uri_)
    }
}

object RemainderNormalizeMatcher {
  def handleProcVar[M[_]](pv: ProcVar, knownFree: DebruijnLevelMap[VarSort])(
      implicit err: MonadError[M, InterpreterError]): M[(Option[Var], DebruijnLevelMap[VarSort])] =
    pv match {
      case pvw: ProcVarWildcard =>
        (Option(Var(Wildcard(Var.WildcardMsg()))), knownFree.addWildcard(pvw.line_num, pvw.col_num))
          .pure[M]
      case pvv: ProcVarVar =>
        knownFree.get(pvv.var_) match {
          case None =>
            val newBindingsPair =
              knownFree.newBinding((pvv.var_, ProcSort, pvv.line_num, pvv.col_num))
            (Option(Var(FreeVar(newBindingsPair._2))), newBindingsPair._1).pure[M]
          case Some((_, _, line, col)) =>
            err.raiseError(
              UnexpectedReuseOfProcContextFree(pvv.var_, line, col, pvv.line_num, pvv.col_num))
        }
    }

  def normalizeMatchProc[M[_]](r: Remainder, knownFree: DebruijnLevelMap[VarSort])(
      implicit err: MonadError[M, InterpreterError]): M[(Option[Var], DebruijnLevelMap[VarSort])] =
    r match {
      case _: RemainderEmpty => (None: Option[Var], knownFree).pure[M]
      case r: RemainderVar =>
        handleProcVar[M](r.procvar_, knownFree)
    }

  def normalizeMatchName[M[_]](nr: NameRemainder, knownFree: DebruijnLevelMap[VarSort])(
      implicit err: MonadError[M, InterpreterError]): M[(Option[Var], DebruijnLevelMap[VarSort])] =
    nr match {
      case _: NameRemainderEmpty => (None: Option[Var], knownFree).pure[M]
      case nr: NameRemainderVar =>
        handleProcVar[M](nr.procvar_, knownFree)
    }
}

object CollectionNormalizeMatcher {
  import scala.collection.JavaConverters._
  def normalizeMatch[M[_]](c: Collection, input: CollectVisitInputs)(
      implicit err: MonadError[M, InterpreterError]): M[CollectVisitOutputs] = {
    def foldMatch[T](knownFree: DebruijnLevelMap[VarSort],
                     listproc: List[Proc],
                     constructor: (Seq[Par], BitSet, Boolean) => T)(
        implicit toExpr: T => Expr): M[CollectVisitOutputs] = {
      val init = (Vector[Par](), knownFree, BitSet(), false)
      listproc
        .foldM(init) { (acc, proc) =>
          ProcNormalizeMatcher
            .normalizeMatch[M](proc, ProcVisitInputs(VectorPar(), input.env, acc._2))
            .map { result =>
              (result.par +: acc._1,
               result.knownFree,
               acc._3 | result.par.locallyFree,
               acc._4 || result.par.connectiveUsed)
            }
        }
        .map { folded =>
          val resultKnownFree = folded._2
          CollectVisitOutputs(constructor(folded._1.reverse, folded._3, folded._4), resultKnownFree)
        }
    }

    def foldMatchMap(listProc: List[AbsynKeyValuePair]): M[CollectVisitOutputs] = {
      val init = (Vector[KeyValuePair](), input.knownFree, BitSet(), false)
      listProc
        .foldM(init) { (acc, e) =>
          e match {
            case e: KeyValuePairImpl =>
              for {
                keyResult <- ProcNormalizeMatcher.normalizeMatch[M](
                              e.proc_1,
                              ProcVisitInputs(VectorPar(), input.env, acc._2))
                valResult <- ProcNormalizeMatcher.normalizeMatch[M](
                              e.proc_2,
                              ProcVisitInputs(VectorPar(), input.env, keyResult.knownFree))
              } yield
                (Vector(KeyValuePair(keyResult.par, valResult.par)) ++ acc._1,
                 valResult.knownFree,
                 acc._3 | keyResult.par.locallyFree | valResult.par.locallyFree,
                 acc._4 || keyResult.par.connectiveUsed || valResult.par.connectiveUsed)
          }
        }
        .map { folded =>
          val resultKnownFree = folded._2
          CollectVisitOutputs(EMap(folded._1.reverse, folded._3, folded._4), resultKnownFree)
        }
    }

    c match {
      case cl: CollectList =>
        RemainderNormalizeMatcher
          .normalizeMatchProc[M](cl.remainder_, input.knownFree)
          .flatMap(remainderResult =>
            foldMatch(remainderResult._2,
                      cl.listproc_.asScala.toList,
                      (ps, lf, wc) =>
                        EList.apply(ps, lf, wc).update(_.optionalRemainder := remainderResult._1)))

      case ct: CollectTuple =>
        val ps = ct.tuple_ match {
          case ts: TupleSingle   => Seq(ts.proc_)
          case tm: TupleMultiple => Seq(tm.proc_) ++ tm.listproc_.asScala.toList
        }
        foldMatch(input.knownFree, ps.toList, ETuple.apply)
      case cs: CollectSet => foldMatch(input.knownFree, cs.listproc_.asScala.toList, ESet.apply)
      case cm: CollectMap => foldMatchMap(cm.listkeyvaluepair_.asScala.toList)
    }
  }
}

object NameNormalizeMatcher {
  def normalizeMatch[M[_]](n: Name, input: NameVisitInputs)(
      implicit err: MonadError[M, InterpreterError]): M[NameVisitOutputs] =
    n match {
      case wc: NameWildcard =>
        val wildcardBindResult = input.knownFree.addWildcard(wc.line_num, wc.col_num)
        NameVisitOutputs(ChanVar(Wildcard(Var.WildcardMsg())), wildcardBindResult).pure[M]
      case n: NameVar =>
        input.env.get(n.var_) match {
          case Some((level, NameSort, _, _)) => {
            NameVisitOutputs(ChanVar(BoundVar(level)), input.knownFree).pure[M]
          }
          case Some((_, ProcSort, line, col)) => {
            err.raiseError(UnexpectedNameContext(n.var_, line, col, n.line_num, n.col_num))
          }
          case None => {
            input.knownFree.get(n.var_) match {
              case None =>
                val newBindingsPair =
                  input.knownFree.newBinding((n.var_, NameSort, n.line_num, n.col_num))
                NameVisitOutputs(ChanVar(FreeVar(newBindingsPair._2)), newBindingsPair._1).pure[M]
              case Some((_, _, line, col)) =>
                err.raiseError(
                  UnexpectedReuseOfNameContextFree(n.var_, line, col, n.line_num, n.col_num))
            }
          }
        }

      case n: NameQuote => {
        def collapseQuoteEval(p: Par): Channel =
          p.singleEval() match {
            case Some(chanNew) => chanNew.get
            case _             => Quote(p)
          }

        ProcNormalizeMatcher
          .normalizeMatch[M](n.proc_, ProcVisitInputs(VectorPar(), input.env, input.knownFree))
          .map(procVisitResult =>
            NameVisitOutputs(collapseQuoteEval(procVisitResult.par), procVisitResult.knownFree))
      }
    }

}

object ProcNormalizeMatcher {
  def normalizeMatch[M[_]](p: Proc, input: ProcVisitInputs)(
      implicit err: MonadError[M, InterpreterError]): M[ProcVisitOutputs] = {
    def unaryExp[T](subProc: Proc, input: ProcVisitInputs, constructor: Option[Par] => T)(
        implicit toExprInstance: T => Expr): M[ProcVisitOutputs] =
      for {
        subResult <- normalizeMatch[M](subProc, input.copy(par = VectorPar()))
      } yield
        ProcVisitOutputs(
          input.par.prepend(constructor(subResult.par)),
          subResult.knownFree
        )

    def binaryExp[T](subProcLeft: Proc,
                     subProcRight: Proc,
                     input: ProcVisitInputs,
                     constructor: (Option[Par], Option[Par]) => T)(
        implicit toExprInstance: T => Expr): M[ProcVisitOutputs] =
      for {
        leftResult <- normalizeMatch[M](subProcLeft, input.copy(par = VectorPar()))
        rightResult <- normalizeMatch[M](
                        subProcRight,
                        input.copy(par = VectorPar(), knownFree = leftResult.knownFree))
      } yield
        ProcVisitOutputs(
          input.par.prepend(constructor(leftResult.par, rightResult.par)),
          rightResult.knownFree
        )

    def normalizeIfElse(valueProc: Proc,
                        trueBodyProc: Proc,
                        falseBodyProc: Proc): M[ProcVisitOutputs] =
      for {
        targetResult <- normalizeMatch[M](valueProc, input)
        trueCaseBody <- normalizeMatch[M](
                         trueBodyProc,
                         ProcVisitInputs(VectorPar(), input.env, targetResult.knownFree))
        falseCaseBody <- normalizeMatch[M](
                          falseBodyProc,
                          ProcVisitInputs(VectorPar(), input.env, trueCaseBody.knownFree))
        desugaredIf = Match(
          targetResult.par,
          Vector(MatchCase(GBool(true), trueCaseBody.par, 0),
                 MatchCase(GBool(false), falseCaseBody.par, 0)),
          targetResult.par.locallyFree | trueCaseBody.par.locallyFree | falseCaseBody.par.locallyFree,
          targetResult.par.connectiveUsed || trueCaseBody.par.connectiveUsed || falseCaseBody.par.connectiveUsed
        )
      } yield ProcVisitOutputs(input.par.prepend(desugaredIf), falseCaseBody.knownFree)

    p match {
      case p: PNegation =>
        normalizeMatch[M](p.proc_,
                          ProcVisitInputs(VectorPar(), input.env, DebruijnLevelMap[VarSort]())).map(
          bodyResult =>
            ProcVisitOutputs(input.par.prepend(Connective(ConnNotBody(bodyResult.par))),
                             input.knownFree))

      case p: PConjunction =>
        for {
          leftResult <- normalizeMatch[M](p.proc_1,
                                          ProcVisitInputs(VectorPar(), input.env, input.knownFree))
          rightResult <- normalizeMatch[M](
                          p.proc_2,
                          ProcVisitInputs(VectorPar(), input.env, leftResult.knownFree))
          lp = leftResult.par
          resultConnective = if (lp.sends.isEmpty && lp.receives.isEmpty && lp.news.isEmpty && lp.exprs.isEmpty && lp.matches.isEmpty && lp.bundles.isEmpty) {
            lp.connectives match {
              case List(Connective(ConnAndBody(ConnectiveBody(ps)))) =>
                Connective(ConnAndBody(ConnectiveBody(ps :+ rightResult.par)))
              case _ => Connective(ConnAndBody(ConnectiveBody(Vector(lp, rightResult.par))))
            }
          } else {
            Connective(ConnAndBody(ConnectiveBody(Vector(lp, rightResult.par))))
          }
        } yield ProcVisitOutputs(input.par.prepend(resultConnective), rightResult.knownFree)

      case p: PDisjunction =>
        for {
          leftResult <- normalizeMatch[M](
                         p.proc_1,
                         ProcVisitInputs(VectorPar(), input.env, DebruijnLevelMap[VarSort]()))
          rightResult <- normalizeMatch[M](
                          p.proc_2,
                          ProcVisitInputs(VectorPar(), input.env, DebruijnLevelMap[VarSort]()))
          lp = leftResult.par
          resultConnective = if (lp.sends.isEmpty && lp.receives.isEmpty && lp.news.isEmpty && lp.exprs.isEmpty && lp.matches.isEmpty && lp.bundles.isEmpty) {
            lp.connectives match {
              case List(Connective(ConnOrBody(ConnectiveBody(ps)))) =>
                Connective(ConnOrBody(ConnectiveBody(ps :+ rightResult.par)))
              case _ => Connective(ConnOrBody(ConnectiveBody(Vector(lp, rightResult.par))))
            }
          } else {
            Connective(ConnOrBody(ConnectiveBody(Vector(lp, rightResult.par))))
          }
        } yield ProcVisitOutputs(input.par.prepend(resultConnective), input.knownFree)

      case p: PGround =>
        ProcVisitOutputs(input.par.prepend(GroundNormalizeMatcher.normalizeMatch(p.ground_)),
                         input.knownFree).pure[M]

      case p: PCollect =>
        CollectionNormalizeMatcher
          .normalizeMatch[M](p.collection_, CollectVisitInputs(input.env, input.knownFree))
          .map(collectResult =>
            ProcVisitOutputs(input.par.prepend(collectResult.expr), collectResult.knownFree))

      case p: PVar =>
        p.procvar_ match {
          case pvv: ProcVarVar =>
            input.env.get(pvv.var_) match {
              case Some((level, ProcSort, _, _)) =>
                ProcVisitOutputs(input.par.prepend(EVar(Some(BoundVar(level)))), input.knownFree)
                  .pure[M]
              case Some((_, NameSort, line, col)) =>
                err.raiseError(
                  UnexpectedProcContext(pvv.var_, line, col, pvv.line_num, pvv.col_num))
              case None =>
                input.knownFree.get(pvv.var_) match {
                  case None =>
                    val newBindingsPair =
                      input.knownFree.newBinding((pvv.var_, ProcSort, pvv.line_num, pvv.col_num))
                    ProcVisitOutputs(
                      input.par.prepend(EVar(FreeVar(newBindingsPair._2))).withConnectiveUsed(true),
                      newBindingsPair._1).pure[M]
                  case Some((_, _, line, col)) =>
                    err.raiseError(
                      UnexpectedReuseOfProcContextFree(pvv.var_,
                                                       line,
                                                       col,
                                                       pvv.line_num,
                                                       pvv.col_num))
                }
            }
          case _: ProcVarWildcard =>
            ProcVisitOutputs(
              input.par.prepend(EVar(Wildcard(Var.WildcardMsg()))).withConnectiveUsed(true),
              input.knownFree.addWildcard(p.line_num, p.col_num)).pure[M]
        }

      case _: PNil => ProcVisitOutputs(input.par, input.knownFree).pure[M]

      case p: PEval =>
        def collapseEvalQuote(chan: Channel): Par =
          chan.channelInstance match {
            case Quote(q) => q
            case _        => Expr(EEvalBody(chan))
          }

        NameNormalizeMatcher
          .normalizeMatch[M](p.name_, NameVisitInputs(input.env, input.knownFree))
          .map(nameMatchResult =>
            ProcVisitOutputs(input.par ++ collapseEvalQuote(nameMatchResult.chan),
                             nameMatchResult.knownFree))

      case p: PMethod => {
        import scala.collection.JavaConverters._
        for {
          targetResult <- normalizeMatch[M](p.proc_, input.copy(par = Par()))
          target       = targetResult.par
          initAcc = (List[Par](),
                     ProcVisitInputs(Par(), input.env, targetResult.knownFree),
                     BitSet(),
                     false)
          argResults <- p.listproc_.asScala.toList.reverse.foldM(initAcc)((acc, e) => {
                         normalizeMatch[M](e, acc._2).map(
                           procMatchResult =>
                             (procMatchResult.par :: acc._1,
                              ProcVisitInputs(Par(), input.env, procMatchResult.knownFree),
                              acc._3 | procMatchResult.par.locallyFree,
                              acc._4 || procMatchResult.par.connectiveUsed))
                       })
        } yield
          ProcVisitOutputs(
            input.par.prepend(
              EMethod(
                p.var_,
                targetResult.par,
                argResults._1,
                target.locallyFree | argResults._3,
                target.connectiveUsed || argResults._4
              )),
            argResults._2.knownFree
          )
      }

      case p: PNot => unaryExp(p.proc_, input, ENot.apply)
      case p: PNeg => unaryExp(p.proc_, input, ENeg.apply)

      case p: PMult  => binaryExp(p.proc_1, p.proc_2, input, EMult.apply)
      case p: PDiv   => binaryExp(p.proc_1, p.proc_2, input, EDiv.apply)
      case p: PAdd   => binaryExp(p.proc_1, p.proc_2, input, EPlus.apply)
      case p: PMinus => binaryExp(p.proc_1, p.proc_2, input, EMinus.apply)

      case p: PLt  => binaryExp(p.proc_1, p.proc_2, input, ELt.apply)
      case p: PLte => binaryExp(p.proc_1, p.proc_2, input, ELte.apply)
      case p: PGt  => binaryExp(p.proc_1, p.proc_2, input, EGt.apply)
      case p: PGte => binaryExp(p.proc_1, p.proc_2, input, EGte.apply)

      case p: PEq  => binaryExp(p.proc_1, p.proc_2, input, EEq.apply)
      case p: PNeq => binaryExp(p.proc_1, p.proc_2, input, ENeq.apply)

      case p: PAnd => binaryExp(p.proc_1, p.proc_2, input, EAnd.apply)
      case p: POr  => binaryExp(p.proc_1, p.proc_2, input, EOr.apply)

      case p: PExprs =>
        normalizeMatch[M](p.proc_, input)

      case p: PSend => {
        import scala.collection.JavaConverters._
        for {
          nameMatchResult <- NameNormalizeMatcher.normalizeMatch[M](
                              p.name_,
                              NameVisitInputs(input.env, input.knownFree))
          initAcc = (Vector[Par](),
                     ProcVisitInputs(VectorPar(), input.env, nameMatchResult.knownFree),
                     BitSet(),
                     false)
          dataResults <- p.listproc_.asScala.toList.reverse.foldM(initAcc)(
                          (acc, e) =>
                            normalizeMatch[M](e, acc._2).map(
                              procMatchResult =>
                                (procMatchResult.par +: acc._1,
                                 ProcVisitInputs(VectorPar(), input.env, procMatchResult.knownFree),
                                 acc._3 | procMatchResult.par.locallyFree,
                                 acc._4 || procMatchResult.par.connectiveUsed)))
          persistent = p.send_ match {
            case _: SendSingle   => false
            case _: SendMultiple => true
          }
        } yield
          ProcVisitOutputs(
            input.par.prepend(
              Send(
                nameMatchResult.chan,
                dataResults._1,
                persistent,
                ChannelLocallyFree.locallyFree(nameMatchResult.chan) | dataResults._3,
                ChannelLocallyFree.connectiveUsed(nameMatchResult.chan) || dataResults._4
              )),
            dataResults._2.knownFree
          )
      }

      case p: PContr => {
        import scala.collection.JavaConverters._
        // A free variable can only be used once in any of the parameters.
        // And we start with the empty free variable map because these free
        // variables aren't free in the surrounding context: they're binders
        for {
          nameMatchResult <- NameNormalizeMatcher
                              .normalizeMatch[M](p.name_,
                                                 NameVisitInputs(input.env, input.knownFree))
          initAcc = (Vector[Channel](), DebruijnLevelMap[VarSort]())
          // Note that we go over these in the order they were given and reverse
          // down below. This is because it makes more sense to number the free
          // variables in the order given, rather than in reverse.
          formalsResults <- p.listname_.asScala.toList.foldM(initAcc)(
                             (acc, n: Name) => {
                               NameNormalizeMatcher
                                 .normalizeMatch[M](n, NameVisitInputs(DebruijnIndexMap(), acc._2))
                                 .map(result => (result.chan +: acc._1, result.knownFree))
                             }
                           )
          remainderResult <- RemainderNormalizeMatcher
                              .normalizeMatchName[M](p.nameremainder_, formalsResults._2)
          newEnv     = input.env.absorbFree(remainderResult._2)._1
          boundCount = remainderResult._2.countNoWildcards
          bodyResult <- ProcNormalizeMatcher.normalizeMatch[M](
                         p.proc_,
                         ProcVisitInputs(VectorPar(), newEnv, nameMatchResult.knownFree))
        } yield
          ProcVisitOutputs(
            input.par.prepend(
              Receive(
                binds = List(
                  ReceiveBind(formalsResults._1.reverse,
                              nameMatchResult.chan,
                              remainderResult._1,
                              boundCount)),
                body = bodyResult.par,
                persistent = true,
                bindCount = boundCount,
                locallyFree = ChannelLocallyFree
                  .locallyFree(nameMatchResult.chan) | (bodyResult.par.locallyFree
                  .from(boundCount)
                  .map(x => x - boundCount)),
                connectiveUsed = ChannelLocallyFree
                  .connectiveUsed(nameMatchResult.chan) || bodyResult.par.connectiveUsed
              )),
            bodyResult.knownFree
          )
      }

      case p: PInput => {
        import scala.collection.JavaConverters._
        // To handle the most common case where we can sort the binds because
        // they're from different sources, Each channel's list of patterns starts its free variables at 0.
        // We check for overlap at the end after sorting. We could check before, but it'd be an extra step.

        // We split this into parts. First we process all the sources, then we process all the bindings.
        def processSources(bindings: List[(List[Name], Name, NameRemainder)])
          : M[(Vector[(List[Name], Channel, NameRemainder)],
               DebruijnLevelMap[VarSort],
               BitSet,
               Boolean)] = {
          val initAcc =
            (Vector[(List[Name], Channel, NameRemainder)](), input.knownFree, BitSet(), false)
          bindings
            .foldM(initAcc)((acc, e) => {
              NameNormalizeMatcher
                .normalizeMatch[M](e._2, NameVisitInputs(input.env, acc._2))
                .map(
                  sourceResult =>
                    ((e._1, sourceResult.chan, e._3) +: acc._1,
                     sourceResult.knownFree,
                     acc._3 | ChannelLocallyFree.locallyFree(sourceResult.chan),
                     acc._4 || ChannelLocallyFree.connectiveUsed(sourceResult.chan)))
            })
            .map(foldResult => (foldResult._1.reverse, foldResult._2, foldResult._3, foldResult._4))
        }
        def processBindings(bindings: Vector[(List[Name], Channel, NameRemainder)])
          : M[Vector[(Vector[Channel], Channel, Option[Var], DebruijnLevelMap[VarSort])]] =
          bindings.traverse {
            case (names: List[Name], chan: Channel, nr: NameRemainder) => {
              val initAcc = (Vector[Channel](), DebruijnLevelMap[VarSort]())
              names
                .foldM(initAcc)((acc, n: Name) => {
                  NameNormalizeMatcher
                    .normalizeMatch[M](n, NameVisitInputs(DebruijnIndexMap(), acc._2))
                    .map(result => (result.chan +: acc._1, result.knownFree))
                })
                .flatMap(formalsResults =>
                  RemainderNormalizeMatcher
                    .normalizeMatchName[M](nr, formalsResults._2)
                    .map(remainderResult =>
                      (formalsResults._1.reverse, chan, remainderResult._1, remainderResult._2)))
            }
          }
        val resM = p.receipt_ match {
          case rl: ReceiptLinear =>
            def bindingsReceiptLinear(
                receipt: ReceiptLinearImpl): M[List[(List[Name], Name, NameRemainder)]] =
              receipt match {
                case ls: LinearSimple =>
                  ls.listlinearbind_.asScala.toList.traverse {
                    case lbi: LinearBindImpl =>
                      (lbi.listname_.asScala.toList, lbi.name_, lbi.nameremainder_).pure[M]
                    case _ =>
                      err.raiseError(
                        UnrecognizedNormalizerError("Unexpected LinearBind production."))
                  }
                case _ =>
                  err.raiseError(
                    UnrecognizedNormalizerError("Unexpected LinearReceipt production."))
              }
            bindingsReceiptLinear(rl.receiptlinearimpl_).map(x => (x, false))
          case rl: ReceiptRepeated =>
            def bindingsRepeatedReceipt(
                receipt: ReceiptRepeatedImpl): M[List[(List[Name], Name, NameRemainder)]] =
              receipt match {
                case ls: RepeatedSimple =>
                  ls.listrepeatedbind_.asScala.toList
                    .traverse {
                      case lbi: RepeatedBindImpl =>
                        (lbi.listname_.asScala.toList, lbi.name_, lbi.nameremainder_).pure[M]
                      case _ =>
                        err.raiseError(
                          UnrecognizedNormalizerError("Unexpected RepeatedBind production."))
                    }
                case _ =>
                  err.raiseError(
                    UnrecognizedNormalizerError("Unexpected RepeatedReceipt production."))
              }
            bindingsRepeatedReceipt(rl.receiptrepeatedimpl_).map(x => (x, true))
        }

        for {
          res                                                              <- resM
          (bindings, persistent)                                           = res
          sourcesP                                                         <- processSources(bindings)
          (sources, thisLevelFree, sourcesLocallyFree, sourcesConnectives) = sourcesP
          bindingsSources                                                  <- processBindings(sources)
          receipts <- ReceiveSortMatcher
                       .preSortBinds[M, VarSort](bindingsSources)
          mergedFrees <- receipts.toList.foldM[M, DebruijnLevelMap[VarSort]](
                          DebruijnLevelMap[VarSort]())((env, receipt) =>
                          env.merge(receipt._2) match {
                            case (newEnv, Nil) => (newEnv: DebruijnLevelMap[VarSort]).pure[M]
                            case (_, (shadowingVar, line, col) :: _) =>
                              val Some((_, _, firstUsageLine, firstUsageCol)) =
                                env.get(shadowingVar)
                              err.raiseError(
                                UnexpectedReuseOfNameContextFree(shadowingVar,
                                                                 firstUsageLine,
                                                                 firstUsageCol,
                                                                 line,
                                                                 col))
                        })
          bindCount  = mergedFrees.countNoWildcards
          binds      = receipts.map(receipt => receipt._1)
          updatedEnv = input.env.absorbFree(mergedFrees)._1
          bodyResult <- normalizeMatch[M](p.proc_,
                                          ProcVisitInputs(VectorPar(), updatedEnv, thisLevelFree))
          connective = sourcesConnectives || bodyResult.par.connectiveUsed
        } yield
          ProcVisitOutputs(
            input.par.prepend(
              Receive(binds,
                      bodyResult.par,
                      persistent,
                      bindCount,
                      sourcesLocallyFree | (bodyResult.par.locallyFree
                        .from(bindCount)
                        .map(x => x - bindCount)),
                      connective)),
            bodyResult.knownFree
          )

      }

      case p: PPar =>
        for {
          result       <- normalizeMatch[M](p.proc_1, input)
          chainedInput = input.copy(knownFree = result.knownFree, par = result.par)
          chainedRes   <- normalizeMatch[M](p.proc_2, chainedInput)
        } yield chainedRes

      case p: PNew => {
        import scala.collection.JavaConverters._
        // TODO: bindings within a single new shouldn't have overlapping names.
        val newBindings = p.listnamedecl_.asScala.toList.map {
          case n: NameDeclSimpl => (n.var_, NameSort, n.line_num, n.col_num)
        }
        val newEnv   = input.env.newBindings(newBindings)
        val newCount = newEnv.count - input.env.count
        normalizeMatch[M](p.proc_, ProcVisitInputs(VectorPar(), newEnv, input.knownFree))
          .map { bodyResult =>
            val foldedNew = bodyResult.par.singleNew() match {
              case Some(New(count, body, locallyFree)) =>
                New(newCount + count, body, locallyFree.from(newCount).map(x => x - newCount))
              case _ =>
                New(newCount,
                    bodyResult.par,
                    bodyResult.par.locallyFree.from(newCount).map(x => x - newCount))
            }
            ProcVisitOutputs(input.par.prepend(foldedNew), bodyResult.knownFree)
          }

      }

      case b: PBundle =>
        def error(targetResult: ProcVisitOutputs): M[ProcVisitOutputs] = {
          val errMsg = {
            def at(variable: String, l: Int, col: Int): String =
              s"$variable line: $l, column: $col"
            val wildcardsPositions = targetResult.knownFree.wildcards.map {
              case (l, col) => at("", l, col)
            }
            val freeVarsPositions = targetResult.knownFree.env.map {
              case (n, (_, _, line, col)) => at(s"`$n`", line, col)
            }
            wildcardsPositions.mkString(" Wildcards at positions: ", ", ", ".") ++
              freeVarsPositions.mkString(" Free variables at positions: ", ", ", ".")
          }
          err.raiseError(
            UnexpectedBundleContent(
              s"Bundle's content shouldn't have free variables or wildcards.$errMsg"))
        }

        import BundleOps._
        for {
          targetResult <- normalizeMatch[M](b.proc_, input.copy(par = VectorPar()))
          outermostBundle = b.bundle_ match {
            case _: BundleReadWrite => Bundle(targetResult.par, writeFlag = true, readFlag = true)
            case _: BundleRead      => Bundle(targetResult.par, writeFlag = false, readFlag = true)
            case _: BundleWrite     => Bundle(targetResult.par, writeFlag = true, readFlag = false)
            case _: BundleEquiv     => Bundle(targetResult.par, writeFlag = false, readFlag = false)
          }
          res <- if (targetResult.par.connectiveUsed) {
                  error(targetResult)
                } else {
                  val newBundle: Bundle = targetResult.par.singleBundle() match {
                    case Some(single) => outermostBundle.merge(single)
                    case None         => outermostBundle
                  }
                  ProcVisitOutputs(input.par.prepend(newBundle), input.knownFree).pure[M]
                }
        } yield res

      case p: PMatch => {
        import scala.collection.JavaConverters._

        def liftCase(c: Case): M[(Proc, Proc)] = c match {
          case ci: CaseImpl => Applicative[M].pure[(Proc, Proc)]((ci.proc_1, ci.proc_2))
          case _ =>
            err.raiseError(UnrecognizedNormalizerError("Unexpected Case implementation."))
        }

        for {
          targetResult <- normalizeMatch[M](p.proc_, input.copy(par = VectorPar()))
          cases        <- p.listcase_.asScala.toList.traverse(liftCase)

          initAcc = (Vector[MatchCase](), targetResult.knownFree, BitSet(), false)
          casesResult <- cases.foldM(initAcc)((acc, caseImpl) =>
                          caseImpl match {
                            case (pattern, caseBody) => {
                              for {
                                patternResult <- normalizeMatch[M](
                                                  pattern,
                                                  ProcVisitInputs(VectorPar(),
                                                                  DebruijnIndexMap[VarSort](),
                                                                  DebruijnLevelMap[VarSort]()))
                                caseEnv    = input.env.absorbFree(patternResult.knownFree)._1
                                boundCount = patternResult.knownFree.countNoWildcards
                                caseBodyResult <- normalizeMatch[M](
                                                   caseBody,
                                                   ProcVisitInputs(VectorPar(), caseEnv, acc._2))
                              } yield
                                (MatchCase(patternResult.par, caseBodyResult.par, boundCount) +: acc._1,
                                 caseBodyResult.knownFree,
                                 acc._3 | caseBodyResult.par.locallyFree
                                   .from(boundCount)
                                   .map(x => x - boundCount),
                                 acc._4 || caseBodyResult.par.connectiveUsed)
                            }
                        })
        } yield
          ProcVisitOutputs(
            input.par.prepend(
              Match(
                targetResult.par,
                casesResult._1.reverse,
                casesResult._3 | targetResult.par.locallyFree,
                casesResult._4 || targetResult.par.connectiveUsed
              )),
            casesResult._2
          )
      }

      case p: PIf     => normalizeIfElse(p.proc_1, p.proc_2, new PNil())
      case p: PIfElse => normalizeIfElse(p.proc_1, p.proc_2, p.proc_3)

      case _ =>
        err.raiseError(UnrecognizedNormalizerError("Compilation of construct not yet supported."))
    }
  }

}

/** Input data to the normalizer
  *
  * @param par collection of things that might be run in parallel
  * @param env
  * @param knownFree
  */
case class ProcVisitInputs(par: Par,
                           env: DebruijnIndexMap[VarSort],
                           knownFree: DebruijnLevelMap[VarSort])
// Returns the update Par and an updated map of free variables.
case class ProcVisitOutputs(par: Par, knownFree: DebruijnLevelMap[VarSort])

case class NameVisitInputs(env: DebruijnIndexMap[VarSort], knownFree: DebruijnLevelMap[VarSort])
case class NameVisitOutputs(chan: Channel, knownFree: DebruijnLevelMap[VarSort])

case class CollectVisitInputs(env: DebruijnIndexMap[VarSort], knownFree: DebruijnLevelMap[VarSort])
case class CollectVisitOutputs(expr: Expr, knownFree: DebruijnLevelMap[VarSort])
