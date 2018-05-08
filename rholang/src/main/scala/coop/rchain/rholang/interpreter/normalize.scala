package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.NormalizerExceptions._
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{
  Ground => AbsynGround,
  KeyValuePair => AbsynKeyValuePair,
  Send => AbsynSend,
  Bundle => AbsynBundle,
  _
}
import implicits._

import scala.collection.immutable.{BitSet, Vector}

sealed trait VarSort
case object ProcSort extends VarSort
case object NameSort extends VarSort

object NormalizerExceptions {
  trait NormalizerException
  case class UnexpectedNameContext(
      varName: String,
      procVarLine: Int,
      procVarCol: Int,
      nameContextLine: Int,
      nameContextCol: Int
  ) extends Exception(
        s"Proc variable: $varName at $procVarLine:$procVarCol used in Name context at $nameContextLine:$nameContextCol")
      with NormalizerException

  case class UnexpectedReuseOfNameContextFree(
      varName: String,
      firstUseLine: Int,
      firstUseCol: Int,
      secondUseLine: Int,
      secondUseCol: Int
  ) extends Exception(
        s"Free variable $varName is used twice as a binder (at $firstUseLine:$firstUseCol and $secondUseLine:$secondUseCol) in name context.")
      with NormalizerException

  case class UnexpectedProcContext(
      varName: String,
      nameVarLine: Int,
      nameVarCol: Int,
      processContextLine: Int,
      processContextCol: Int
  ) extends Exception(
        s"Name variable: $varName at $nameVarLine:$nameVarCol used in process context at $processContextLine:$processContextCol")
      with NormalizerException

  case class UnexpectedReuseOfProcContextFree(
      varName: String,
      firstUseLine: Int,
      firstUseCol: Int,
      secondUseLine: Int,
      secondUseCol: Int
  ) extends Exception(
        s"Free variable $varName is used twice as a binder (at $firstUseLine:$firstUseCol and $secondUseLine:$secondUseCol) in process context.")
      with NormalizerException

  final case class UnexpectedBundleContent(msg: String)
      extends Exception(msg)
      with NormalizerException
}

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
  def handleProcVar(
      pv: ProcVar,
      knownFree: DebruijnLevelMap[VarSort]): (Option[Var], DebruijnLevelMap[VarSort]) =
    pv match {
      case pvw: ProcVarWildcard =>
        (Some(Var(Wildcard(Var.WildcardMsg()))), knownFree.addWildcard(pvw.line_num, pvw.col_num))
      case pvv: ProcVarVar =>
        knownFree.get(pvv.var_) match {
          case None =>
            val newBindingsPair =
              knownFree.newBinding((pvv.var_, ProcSort, pvv.line_num, pvv.col_num))
            (Some(FreeVar(newBindingsPair._2)), newBindingsPair._1)
          case Some((_, _, line, col)) =>
            throw UnexpectedReuseOfProcContextFree(pvv.var_, line, col, pvv.line_num, pvv.col_num)
        }
    }

  def normalizeMatchProc(
      r: Remainder,
      knownFree: DebruijnLevelMap[VarSort]): (Option[Var], DebruijnLevelMap[VarSort]) =
    r match {
      case _: RemainderEmpty => (None, knownFree)
      case r: RemainderVar =>
        handleProcVar(r.procvar_, knownFree)
    }

  def normalizeMatchName(
      nr: NameRemainder,
      knownFree: DebruijnLevelMap[VarSort]): (Option[Var], DebruijnLevelMap[VarSort]) =
    nr match {
      case _: NameRemainderEmpty => (None, knownFree)
      case nr: NameRemainderVar =>
        handleProcVar(nr.procvar_, knownFree)
    }
}

object CollectionNormalizeMatcher {
  import scala.collection.JavaConverters._
  def normalizeMatch(c: Collection, input: CollectVisitInputs): CollectVisitOutputs = {
    def foldMatch[T](knownFree: DebruijnLevelMap[VarSort],
                     listproc: List[Proc],
                     constructor: (Seq[Par], BitSet, Boolean) => T)(
        implicit toExpr: T => Expr): CollectVisitOutputs = {
      val folded = ((Vector[Par](), knownFree, BitSet(), false) /: listproc)(
        (acc, e) => {
          val result =
            ProcNormalizeMatcher.normalizeMatch(e, ProcVisitInputs(VectorPar(), input.env, acc._2))
          (result.par +: acc._1,
           result.knownFree,
           acc._3 | result.par.locallyFree,
           acc._4 || result.par.connectiveUsed)
        }
      )
      val resultKnownFree = folded._2
      CollectVisitOutputs(constructor(folded._1.reverse, folded._3, folded._4), resultKnownFree)
    }

    def foldMatchMap(listproc: List[AbsynKeyValuePair]): CollectVisitOutputs = {
      val folded = ((Seq[KeyValuePair](), input.knownFree, BitSet(), false) /: listproc)(
        (acc, e) => {
          e match {
            case e: KeyValuePairImpl => {
              val keyResult =
                ProcNormalizeMatcher
                  .normalizeMatch(e.proc_1, ProcVisitInputs(VectorPar(), input.env, acc._2))
              val valResult = ProcNormalizeMatcher
                .normalizeMatch(e.proc_2,
                                ProcVisitInputs(VectorPar(), input.env, keyResult.knownFree))
              (Seq(KeyValuePair(keyResult.par, valResult.par)) ++ acc._1,
               valResult.knownFree,
               acc._3 | keyResult.par.locallyFree | valResult.par.locallyFree,
               acc._4 || keyResult.par.connectiveUsed || valResult.par.connectiveUsed)
            }
          }
        }
      )
      val resultKnownFree = folded._2
      CollectVisitOutputs(EMap(folded._1.reverse, folded._3, folded._4), resultKnownFree)
    }
    c match {
      case cl: CollectList =>
        val remainderResult =
          RemainderNormalizeMatcher.normalizeMatchProc(cl.remainder_, input.knownFree)
        foldMatch(
          remainderResult._2,
          cl.listproc_.asScala.toList,
          (ps, lf, wc) => EList.apply(ps, lf, wc).update(_.optionalRemainder := remainderResult._1))
      case ct: CollectTuple => foldMatch(input.knownFree, ct.listproc_.asScala.toList, ETuple.apply)
      case cs: CollectSet   => foldMatch(input.knownFree, cs.listproc_.asScala.toList, ESet.apply)
      case cm: CollectMap   => foldMatchMap(cm.listkeyvaluepair_.asScala.toList)
    }
  }
}

object NameNormalizeMatcher {
  def normalizeMatch(n: Name, input: NameVisitInputs): NameVisitOutputs =
    n match {
      case wc: NameWildcard =>
        val wildcardBindResult = input.knownFree.addWildcard(wc.line_num, wc.col_num)
        NameVisitOutputs(ChanVar(Wildcard(Var.WildcardMsg())), wildcardBindResult)
      case n: NameVar =>
        input.env.get(n.var_) match {
          case Some((level, NameSort, _, _)) => {
            NameVisitOutputs(ChanVar(BoundVar(level)), input.knownFree)
          }
          case Some((_, ProcSort, line, col)) => {
            throw UnexpectedNameContext(n.var_, line, col, n.line_num, n.col_num)
          }
          case None => {
            input.knownFree.get(n.var_) match {
              case None =>
                val newBindingsPair =
                  input.knownFree.newBinding((n.var_, NameSort, n.line_num, n.col_num))
                NameVisitOutputs(ChanVar(FreeVar(newBindingsPair._2)), newBindingsPair._1)
              case Some((_, _, line, col)) =>
                throw UnexpectedReuseOfNameContextFree(n.var_, line, col, n.line_num, n.col_num)
            }
          }
        }

      case n: NameQuote => {
        def collapseQuoteEval(p: Par): Channel =
          p.singleEval() match {
            case Some(Eval(chanNew)) => chanNew.get
            case _                   => Quote(p)
          }

        val procVisitResult: ProcVisitOutputs = ProcNormalizeMatcher.normalizeMatch(
          n.proc_,
          ProcVisitInputs(VectorPar(), input.env, input.knownFree))

        NameVisitOutputs(collapseQuoteEval(procVisitResult.par), procVisitResult.knownFree)
      }
    }

}

object ProcNormalizeMatcher {
  def normalizeMatch(p: Proc, input: ProcVisitInputs): ProcVisitOutputs = {
    def unaryExp[T](subProc: Proc, input: ProcVisitInputs, constructor: Option[Par] => T)(
        implicit toExprInstance: T => Expr): ProcVisitOutputs = {
      val subResult    = normalizeMatch(subProc, input.copy(par = VectorPar()))
      val subResultPar = subResult.par
      ProcVisitOutputs(
        input.par.prepend(constructor(subResultPar)),
        subResult.knownFree
      )
    }
    def binaryExp[T](subProcLeft: Proc,
                     subProcRight: Proc,
                     input: ProcVisitInputs,
                     constructor: (Option[Par], Option[Par]) => T)(
        implicit toExprInstance: T => Expr): ProcVisitOutputs = {
      val leftResult = normalizeMatch(subProcLeft, input.copy(par = VectorPar()))
      val rightResult =
        normalizeMatch(subProcRight,
                       input.copy(par = VectorPar(), knownFree = leftResult.knownFree))
      ProcVisitOutputs(
        input.par.prepend(constructor(leftResult.par, rightResult.par)),
        rightResult.knownFree
      )
    }
    def normalizeIfElse(valueProc: Proc, trueBodyProc: Proc, falseBodyProc: Proc) = {
      import scala.collection.JavaConverters._

      val targetResult = normalizeMatch(valueProc, input)
      val trueCaseBody =
        normalizeMatch(trueBodyProc,
                       ProcVisitInputs(VectorPar(), input.env, targetResult.knownFree))
      val falseCaseBody =
        normalizeMatch(falseBodyProc,
                       ProcVisitInputs(VectorPar(), input.env, trueCaseBody.knownFree))

      val desugaredIf = Match(
        targetResult.par,
        Vector(MatchCase(GBool(true), trueCaseBody.par, 0),
               MatchCase(GBool(false), falseCaseBody.par, 0)),
        targetResult.par.locallyFree | trueCaseBody.par.locallyFree | falseCaseBody.par.locallyFree,
        targetResult.par.connectiveUsed || trueCaseBody.par.connectiveUsed || falseCaseBody.par.connectiveUsed
      )
      ProcVisitOutputs(input.par.prepend(desugaredIf), falseCaseBody.knownFree)
    }

    p match {
      case p: PGround =>
        ProcVisitOutputs(input.par.prepend(GroundNormalizeMatcher.normalizeMatch(p.ground_)),
                         input.knownFree)

      case p: PCollect => {
        val collectResult = CollectionNormalizeMatcher.normalizeMatch(
          p.collection_,
          CollectVisitInputs(input.env, input.knownFree))
        ProcVisitOutputs(input.par.prepend(collectResult.expr), collectResult.knownFree)
      }

      case p: PVar =>
        p.procvar_ match {
          case pvv: ProcVarVar =>
            input.env.get(pvv.var_) match {
              case Some((level, ProcSort, _, _)) =>
                ProcVisitOutputs(input.par.prepend(EVar(Some(BoundVar(level)))), input.knownFree)
              case Some((_, NameSort, line, col)) =>
                throw UnexpectedProcContext(pvv.var_, line, col, pvv.line_num, pvv.col_num)
              case None =>
                input.knownFree.get(pvv.var_) match {
                  case None =>
                    val newBindingsPair =
                      input.knownFree.newBinding((pvv.var_, ProcSort, pvv.line_num, pvv.col_num))
                    ProcVisitOutputs(
                      input.par.prepend(EVar(FreeVar(newBindingsPair._2))).withConnectiveUsed(true),
                      newBindingsPair._1)
                  case Some((_, _, line, col)) =>
                    throw UnexpectedReuseOfProcContextFree(pvv.var_,
                                                           line,
                                                           col,
                                                           pvv.line_num,
                                                           pvv.col_num)
                }
            }
          case _: ProcVarWildcard =>
            ProcVisitOutputs(
              input.par.prepend(EVar(Wildcard(Var.WildcardMsg()))).withConnectiveUsed(true),
              input.knownFree.addWildcard(p.line_num, p.col_num))
        }

      case _: PNil => ProcVisitOutputs(input.par, input.knownFree)

      case p: PEval =>
        def collapseEvalQuote(chan: Channel): Par =
          chan.channelInstance match {
            case Quote(q) => q
            case _        => Eval(chan)
          }

        val nameMatchResult =
          NameNormalizeMatcher.normalizeMatch(p.name_, NameVisitInputs(input.env, input.knownFree))
        ProcVisitOutputs(input.par ++ collapseEvalQuote(nameMatchResult.chan),
                         nameMatchResult.knownFree)

      case p: PMethod => {
        import scala.collection.JavaConverters._
        val targetResult = normalizeMatch(p.proc_, input.copy(par = Par()))
        val target       = targetResult.par
        val method       = p.var_
        val initAcc =
          (List[Par](), ProcVisitInputs(Par(), input.env, targetResult.knownFree), BitSet(), false)
        val argResults = (initAcc /: p.listproc_.asScala.toList.reverse)(
          (acc, e) => {
            val procMatchResult = normalizeMatch(e, acc._2)
            (procMatchResult.par :: acc._1,
             ProcVisitInputs(Par(), input.env, procMatchResult.knownFree),
             acc._3 | procMatchResult.par.locallyFree,
             acc._4 || procMatchResult.par.connectiveUsed)
          }
        )
        ProcVisitOutputs(
          input.par.prepend(
            EMethod(
              method,
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

      case p: PSend => {
        import scala.collection.JavaConverters._
        val nameMatchResult =
          NameNormalizeMatcher.normalizeMatch(p.name_, NameVisitInputs(input.env, input.knownFree))
        val initAcc =
          (Vector[Par](),
           ProcVisitInputs(VectorPar(), input.env, nameMatchResult.knownFree),
           BitSet(),
           false)
        val dataResults = (initAcc /: p.listproc_.asScala.toList.reverse)(
          (acc, e) => {
            val procMatchResult = normalizeMatch(e, acc._2)
            (procMatchResult.par +: acc._1,
             ProcVisitInputs(VectorPar(), input.env, procMatchResult.knownFree),
             acc._3 | procMatchResult.par.locallyFree,
             acc._4 || procMatchResult.par.connectiveUsed)
          }
        )
        val persistent = p.send_ match {
          case _: SendSingle   => false
          case _: SendMultiple => true
        }
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
        // variables aren't free in the surrounding context: they're binders.
        val initAcc = (Vector[Channel](), DebruijnLevelMap[VarSort]())
        val nameMatchResult =
          NameNormalizeMatcher.normalizeMatch(p.name_, NameVisitInputs(input.env, input.knownFree))
        // Note that we go over these in the order they were given and reverse
        // down below. This is because it makes more sense to number the free
        // variables in the order given, rather than in reverse.
        val formalsResults = (initAcc /: p.listname_.asScala.toList)(
          (acc, n: Name) => {
            val result =
              NameNormalizeMatcher.normalizeMatch(n, NameVisitInputs(DebruijnIndexMap(), acc._2))
            (result.chan +: acc._1, result.knownFree)
          }
        )
        val remainderResult =
          RemainderNormalizeMatcher.normalizeMatchName(p.nameremainder_, formalsResults._2)
        val newEnv     = input.env.absorbFree(remainderResult._2)._1
        val boundCount = remainderResult._2.countNoWildcards
        val bodyResult = ProcNormalizeMatcher.normalizeMatch(
          p.proc_,
          ProcVisitInputs(VectorPar(), newEnv, nameMatchResult.knownFree))
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
          : (Vector[(List[Name], Channel, NameRemainder)],
             DebruijnLevelMap[VarSort],
             BitSet,
             Boolean) = {
          val initAcc =
            (Vector[(List[Name], Channel, NameRemainder)](), input.knownFree, BitSet(), false)
          val foldResult = (initAcc /: bindings)((acc, e) => {
            val sourceResult =
              NameNormalizeMatcher.normalizeMatch(e._2, NameVisitInputs(input.env, acc._2))
            ((e._1, sourceResult.chan, e._3) +: acc._1,
             sourceResult.knownFree,
             acc._3 | ChannelLocallyFree.locallyFree(sourceResult.chan),
             acc._4 || ChannelLocallyFree.connectiveUsed(sourceResult.chan))
          })
          (foldResult._1.reverse, foldResult._2, foldResult._3, foldResult._4)
        }
        def processBindings(bindings: Vector[(List[Name], Channel, NameRemainder)])
          : Vector[(Vector[Channel], Channel, Option[Var], DebruijnLevelMap[VarSort])] =
          bindings map {
            case (names: List[Name], chan: Channel, nr: NameRemainder) => {
              val initAcc = (Vector[Channel](), DebruijnLevelMap[VarSort]())
              val formalsResults = (initAcc /: names)(
                (acc, n: Name) => {
                  val result =
                    NameNormalizeMatcher.normalizeMatch(n,
                                                        NameVisitInputs(DebruijnIndexMap(), acc._2))
                  (result.chan +: acc._1, result.knownFree)
                }
              )
              val remainderResult =
                RemainderNormalizeMatcher.normalizeMatchName(nr, formalsResults._2)
              (formalsResults._1.reverse, chan, remainderResult._1, remainderResult._2)
            }
          }
        val (bindings, persistent) = p.receipt_ match {
          case rl: ReceiptLinear =>
            rl.receiptlinearimpl_ match {
              case ls: LinearSimple =>
                (ls.listlinearbind_.asScala.toList.map {
                  case lbi: LinearBindImpl =>
                    (lbi.listname_.asScala.toList, lbi.name_, lbi.nameremainder_)
                  case _ => throw new Error("Unexpected LinearBind production.")
                }, false)
              case _ => throw new Error("Unexpected LinearReceipt production.")
            }
          case rl: ReceiptRepeated =>
            rl.receiptrepeatedimpl_ match {
              case ls: RepeatedSimple =>
                (ls.listrepeatedbind_.asScala.toList.map {
                  case lbi: RepeatedBindImpl =>
                    (lbi.listname_.asScala.toList, lbi.name_, lbi.nameremainder_)
                  case _ => throw new Error("Unexpected RepeatedBind production.")
                }, true)
              case _ => throw new Error("Unexpected RepeatedReceipt production.")
            }
        }
        val (sources, thisLevelFree, sourcesLocallyFree, sourcesConnectives) = processSources(
          bindings)
        val receipts = ReceiveSortMatcher.preSortBinds(processBindings(sources))
        val mergedFrees = (DebruijnLevelMap[VarSort]() /: receipts)((env, receipt) =>
          env.merge(receipt._2) match {
            case (newEnv, Nil) => newEnv
            case (_, (shadowingVar, line, col) :: _) =>
              val Some((_, _, firstUsageLine, firstUsageCol)) = env.get(shadowingVar)
              throw UnexpectedReuseOfNameContextFree(shadowingVar,
                                                     firstUsageLine,
                                                     firstUsageCol,
                                                     line,
                                                     col)
        })
        val bindCount  = mergedFrees.countNoWildcards
        val binds      = receipts.map(receipt => receipt._1)
        val updatedEnv = input.env.absorbFree(mergedFrees)._1
        val bodyResult =
          normalizeMatch(p.proc_, ProcVisitInputs(VectorPar(), updatedEnv, thisLevelFree))
        val connective = sourcesConnectives || bodyResult.par.connectiveUsed
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

      case p: PPar => {
        val result       = normalizeMatch(p.proc_1, input)
        val chainedInput = input.copy(knownFree = result.knownFree, par = result.par)
        normalizeMatch(p.proc_2, chainedInput)
      }

      case p: PNew => {
        import scala.collection.JavaConverters._
        // TODO: bindings within a single new shouldn't have overlapping names.
        val newBindings = p.listnamedecl_.asScala.toList.map {
          case n: NameDeclSimpl => (n.var_, NameSort, n.line_num, n.col_num)
        }
        val newEnv   = input.env.newBindings(newBindings)
        val newCount = newEnv.count - input.env.count
        val bodyResult =
          normalizeMatch(p.proc_, ProcVisitInputs(VectorPar(), newEnv, input.knownFree))
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

      case b: PBundle =>
        def error(targetResult: ProcVisitOutputs): ProcVisitOutputs = {
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
          throw UnexpectedBundleContent(
            s"Bundle's content shouldn't have free variables or wildcards.$errMsg")
        }

        val targetResult = normalizeMatch(b.proc_, input.copy(par = VectorPar()))
        val outermostBundle = b.bundle_ match {
          case _: BundleReadWrite => Bundle(targetResult.par, writeFlag = true, readFlag = true)
          case _: BundleRead      => Bundle(targetResult.par, writeFlag = false, readFlag = true)
          case _: BundleWrite     => Bundle(targetResult.par, writeFlag = true, readFlag = false)
          case _: BundleEquiv     => Bundle(targetResult.par, writeFlag = false, readFlag = false)
        }

        import BundleOps._

        if (targetResult.par.connectiveUsed) {
          error(targetResult)
        } else {
          val newBundle: Bundle = targetResult.par.singleBundle() match {
            case Some(single) => outermostBundle.merge(single)
            case None         => outermostBundle
          }
          ProcVisitOutputs(input.par.prepend(newBundle), input.knownFree)
        }

      case p: PMatch => {
        import scala.collection.JavaConverters._

        val targetResult = normalizeMatch(p.proc_, input.copy(par = VectorPar()))
        val cases = p.listcase_.asScala.toList.map {
          case ci: CaseImpl => (ci.proc_1, ci.proc_2)
          case _            => throw new Error("Unexpected Case implementation.")
        }

        val initAcc = (Vector[MatchCase](), targetResult.knownFree, BitSet(), false)
        val casesResult = (initAcc /: cases) { (acc, caseImpl) =>
          caseImpl match {
            case (pattern, caseBody) => {
              val patternResult =
                normalizeMatch(pattern,
                               ProcVisitInputs(VectorPar(),
                                               DebruijnIndexMap[VarSort](),
                                               DebruijnLevelMap[VarSort]()))
              val caseEnv    = input.env.absorbFree(patternResult.knownFree)._1
              val boundCount = patternResult.knownFree.countNoWildcards
              val caseBodyResult =
                normalizeMatch(caseBody, ProcVisitInputs(VectorPar(), caseEnv, acc._2))
              (MatchCase(patternResult.par, caseBodyResult.par, boundCount) +: acc._1,
               caseBodyResult.knownFree,
               acc._3 | caseBodyResult.par.locallyFree.from(boundCount).map(x => x - boundCount),
               acc._4 || caseBodyResult.par.connectiveUsed)
            }
          }
        }
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

      case _ => throw new Error("Compilation of construct not yet supported.")
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
