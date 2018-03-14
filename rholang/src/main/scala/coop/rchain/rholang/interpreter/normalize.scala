package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{
  Ground => AbsynGround,
  KeyValuePair => AbsynKeyValuePair,
  Send => AbsynSend,
  _
}
import implicits._

import scala.collection.immutable.BitSet

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

object CollectionNormalizeMatcher {
  import scala.collection.JavaConverters._
  def normalizeMatch(c: Collection, input: CollectVisitInputs): CollectVisitOutputs = {
    def foldMatch[T](listproc: List[Proc], constructor: (Seq[Par], Int, BitSet) => T)(
        implicit toExpr: T => Expr): CollectVisitOutputs = {
      val folded = ((List[Par](), input.knownFree, BitSet()) /: listproc)(
        (acc, e) => {
          val result =
            ProcNormalizeMatcher.normalizeMatch(e, ProcVisitInputs(Par(), input.env, acc._2))
          (result.par :: acc._1, result.knownFree, acc._3 | result.par.locallyFree)
        }
      )
      val resultKnownFree = folded._2
      val freeCount       = resultKnownFree.count - input.knownFree.count
      CollectVisitOutputs(constructor(folded._1.reverse, freeCount, folded._3), resultKnownFree)
    }

    def foldMatchMap(listproc: List[AbsynKeyValuePair]): CollectVisitOutputs = {
      val folded = ((Seq[KeyValuePair](), input.knownFree, BitSet()) /: listproc)(
        (acc, e) => {
          e match {
            case e: KeyValuePairImpl => {
              val keyResult =
                ProcNormalizeMatcher.normalizeMatch(e.proc_1,
                                                    ProcVisitInputs(Par(), input.env, acc._2))
              val valResult = ProcNormalizeMatcher
                .normalizeMatch(e.proc_2, ProcVisitInputs(Par(), input.env, keyResult.knownFree))
              (Seq(KeyValuePair(keyResult.par, valResult.par)) ++ acc._1,
               valResult.knownFree,
               acc._3 | keyResult.par.locallyFree | valResult.par.locallyFree)
            }
          }
        }
      )
      val resultKnownFree = folded._2
      val freeCount       = resultKnownFree.count - input.knownFree.count
      CollectVisitOutputs(EMap(folded._1.reverse, freeCount, folded._3), resultKnownFree)
    }
    c match {
      case cl: CollectList  => foldMatch(cl.listproc_.asScala.toList, EList.apply)
      case ct: CollectTuple => foldMatch(ct.listproc_.asScala.toList, ETuple.apply)
      case cs: CollectSet   => foldMatch(cs.listproc_.asScala.toList, ESet.apply)
      case cm: CollectMap   => foldMatchMap(cm.listkeyvaluepair_.asScala.toList)
    }
  }
}

object NameNormalizeMatcher {
  def normalizeMatch(n: Name, input: NameVisitInputs): NameVisitOutputs =
    n match {
      case _: NameWildcard =>
        val wildcardBindResult = input.knownFree.setWildcardUsed(1)
        NameVisitOutputs(ChanVar(Wildcard(Var.WildcardMsg())), wildcardBindResult)
      case n: NameVar =>
        input.env.get(n.var_) match {
          case Some((level, NameSort)) => {
            NameVisitOutputs(ChanVar(BoundVar(level)), input.knownFree)
          }
          case Some((_, ProcSort)) => {
            throw new Error("Proc variable used in name context.")
          }
          case None => {
            input.knownFree.get(n.var_) match {
              case None =>
                val newBindingsPair =
                  input.knownFree.newBinding((n.var_, NameSort))
                NameVisitOutputs(ChanVar(FreeVar(newBindingsPair._2)), newBindingsPair._1)
              case _ => throw new Error("Free variable used as binder may not be used twice.")
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
          ProcVisitInputs(Par(), input.env, input.knownFree))

        NameVisitOutputs(collapseQuoteEval(procVisitResult.par), procVisitResult.knownFree)
      }
    }
}

object ProcNormalizeMatcher {
  def normalizeMatch(p: Proc, input: ProcVisitInputs): ProcVisitOutputs = {
    def unaryExp[T](subProc: Proc, input: ProcVisitInputs, constructor: Option[Par] => T)(
        implicit toExprInstance: T => Expr): ProcVisitOutputs = {
      val subResult    = normalizeMatch(subProc, input.copy(par = Par()))
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
      val leftResult = normalizeMatch(subProcLeft, input.copy(par = Par()))
      val rightResult =
        normalizeMatch(subProcRight, input.copy(par = Par(), knownFree = leftResult.knownFree))
      ProcVisitOutputs(
        input.par.prepend(constructor(leftResult.par, rightResult.par)),
        rightResult.knownFree
      )
    }
    def normalizeIfElse(valueProc: Proc, trueBodyProc: Proc, falseBodyProc: Proc) = {
      import scala.collection.JavaConverters._

      val targetResult = normalizeMatch(valueProc, input)
      val trueCaseBody =
        normalizeMatch(trueBodyProc, ProcVisitInputs(Par(), input.env, targetResult.knownFree))
      val falseCaseBody =
        normalizeMatch(falseBodyProc, ProcVisitInputs(Par(), input.env, trueCaseBody.knownFree))
      val freeCount = falseCaseBody.knownFree.count - input.knownFree.count

      val desugaredIf = Match(
        targetResult.par,
        List(MatchCase(GBool(true), trueCaseBody.par), MatchCase(GBool(false), falseCaseBody.par)),
        freeCount,
        targetResult.par.locallyFree | trueCaseBody.par.locallyFree | falseCaseBody.par.locallyFree
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
              case Some((level, ProcSort)) => {
                ProcVisitOutputs(input.par.prepend(EVar(Some(BoundVar(level)))), input.knownFree)
              }
              case Some((_, NameSort)) => {
                throw new Error("Name variable used in process context.")
              }
              case None => {
                input.knownFree.get(pvv.var_) match {
                  case None =>
                    val newBindingsPair =
                      input.knownFree.newBinding((pvv.var_, ProcSort))
                    ProcVisitOutputs(input.par.prepend(EVar(FreeVar(newBindingsPair._2))),
                                     newBindingsPair._1)
                  case _ => throw new Error("Free variable used as binder may not be used twice.")
                }
              }
            }
          case _: ProcVarWildcard =>
            ProcVisitOutputs(input.par.prepend(EVar(Wildcard(Var.WildcardMsg()))),
                             input.knownFree.setWildcardUsed(1))
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
          (List[Par](), ProcVisitInputs(Par(), input.env, nameMatchResult.knownFree), BitSet())
        val dataResults = (initAcc /: p.listproc_.asScala.toList.reverse)(
          (acc, e) => {
            val procMatchResult = normalizeMatch(e, acc._2)
            (procMatchResult.par :: acc._1,
             ProcVisitInputs(Par(), input.env, procMatchResult.knownFree),
             acc._3 | procMatchResult.par.locallyFree)
          }
        )
        val persistent = p.send_ match {
          case _: SendSingle   => false
          case _: SendMultiple => true
        }
        val freeCount = dataResults._2.knownFree.count - input.knownFree.count
        ProcVisitOutputs(
          input.par.prepend(
            Send(nameMatchResult.chan,
                 dataResults._1,
                 persistent,
                 freeCount,
                 ChannelLocallyFree.locallyFree(nameMatchResult.chan) | dataResults._3)),
          dataResults._2.knownFree
        )
      }

      case p: PContr => {
        import scala.collection.JavaConverters._
        // A free variable can only be used once in any of the parameters.
        // And we start with the empty free variable map because these free
        // variables aren't free in the surrounding context: they're binders.
        val initAcc = (List[Channel](), DebruijnLevelMap[VarSort]())
        val nameMatchResult =
          NameNormalizeMatcher.normalizeMatch(p.name_, NameVisitInputs(input.env, input.knownFree))
        // Note that we go over these in the order they were given and reverse
        // down below. This is because it makes more sense to number the free
        // variables in the order given, rather than in reverse.
        val formalsResults = (initAcc /: p.listname_.asScala.toList)(
          (acc, n: Name) => {
            val result =
              NameNormalizeMatcher.normalizeMatch(n, NameVisitInputs(DebruijnLevelMap(), acc._2))
            (result.chan :: acc._1, result.knownFree)
          }
        )
        val newEnv     = input.env.absorbFree(formalsResults._2)._1
        val boundCount = formalsResults._2.countNoWildcards
        val bodyResult = ProcNormalizeMatcher.normalizeMatch(
          p.proc_,
          ProcVisitInputs(Par(), newEnv, nameMatchResult.knownFree))
        val freeCount = bodyResult.knownFree.count - input.knownFree.count
        ProcVisitOutputs(
          input.par.prepend(
            Receive(
              List(ReceiveBind(formalsResults._1.reverse, nameMatchResult.chan)),
              bodyResult.par,
              true,
              boundCount,
              freeCount,
              (ChannelLocallyFree.locallyFree(nameMatchResult.chan) | bodyResult.par.locallyFree)
                .until(input.env.next)
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
        def processSources(bindings: List[(List[Name], Name)])
          : (List[(List[Name], Channel)], DebruijnLevelMap[VarSort], BitSet) = {
          val initAcc = (List[(List[Name], Channel)](), input.knownFree, BitSet())
          val foldResult = (initAcc /: bindings)((acc, e) => {
            val sourceResult =
              NameNormalizeMatcher.normalizeMatch(e._2, NameVisitInputs(input.env, acc._2))
            ((e._1, sourceResult.chan) :: acc._1,
             sourceResult.knownFree,
             acc._3 | ChannelLocallyFree.locallyFree(sourceResult.chan))
          })
          (foldResult._1.reverse, foldResult._2, foldResult._3)
        }
        def processBindings(bindings: List[(List[Name], Channel)])
          : List[(List[Channel], Channel, DebruijnLevelMap[VarSort])] =
          bindings map {
            case (names: List[Name], chan: Channel) => {
              val initAcc = (List[Channel](), DebruijnLevelMap[VarSort]())
              val formalsResults = (initAcc /: names)(
                (acc, n: Name) => {
                  val result =
                    NameNormalizeMatcher.normalizeMatch(n,
                                                        NameVisitInputs(DebruijnLevelMap(), acc._2))
                  (result.chan :: acc._1, result.knownFree)
                }
              )
              (formalsResults._1.reverse, chan, formalsResults._2)
            }
          }
        val (bindings, persistent) = p.receipt_ match {
          case rl: ReceiptLinear =>
            rl.receiptlinearimpl_ match {
              case ls: LinearSimple =>
                (ls.listlinearbind_.asScala.toList.map {
                  case lbi: LinearBindImpl => (lbi.listname_.asScala.toList, lbi.name_)
                  case _                   => throw new Error("Unexpected LinearBind production.")
                }, false)
              case _ => throw new Error("Unexpected LinearReceipt production.")
            }
          case rl: ReceiptRepeated =>
            rl.receiptrepeatedimpl_ match {
              case ls: RepeatedSimple =>
                (ls.listrepeatedbind_.asScala.toList.map {
                  case lbi: RepeatedBindImpl => (lbi.listname_.asScala.toList, lbi.name_)
                  case _                     => throw new Error("Unexpected RepeatedBind production.")
                }, true)
              case _ => throw new Error("Unexpected RepeatedReceipt production.")
            }
        }
        val (sources, thisLevelFree, sourcesLocallyFree) = processSources(bindings)
        val receipts                                     = ReceiveSortMatcher.preSortBinds(processBindings(sources))
        val mergedFrees = (DebruijnLevelMap[VarSort]() /: receipts)((env, receipt) =>
          env.absorbFree(receipt._3) match {
            case (newEnv, Nil) => newEnv
            case _ =>
              throw new Error("Free variable used as binder may not be used twice.")
        })
        val bindCount  = mergedFrees.countNoWildcards
        val binds      = receipts.map((receipt) => ReceiveBind(receipt._1, receipt._2))
        val updatedEnv = input.env.absorbFree(mergedFrees)._1
        val bodyResult =
          normalizeMatch(p.proc_, ProcVisitInputs(Par(), updatedEnv, thisLevelFree))
        val freeCount = bodyResult.knownFree.count - input.knownFree.count
        ProcVisitOutputs(
          input.par.prepend(
            Receive(binds,
                    bodyResult.par,
                    persistent,
                    bindCount,
                    freeCount,
                    (sourcesLocallyFree | bodyResult.par.locallyFree).until(input.env.next))),
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
          case n: NameDeclSimpl => (n.var_, NameSort)
        }
        val newEnv     = input.env.newBindings(newBindings)._1
        val newCount   = newEnv.count - input.env.count
        val bodyResult = normalizeMatch(p.proc_, ProcVisitInputs(Par(), newEnv, input.knownFree))
        val foldedNew = bodyResult.par.singleNew() match {
          case Some(New(count, body, locallyFree)) =>
            New(newCount + count, body, locallyFree.until(input.env.next))
          case _ =>
            New(newCount, bodyResult.par, bodyResult.par.locallyFree.until(input.env.next))
        }
        ProcVisitOutputs(input.par.prepend(foldedNew), bodyResult.knownFree)
      }

      case p: PMatch => {
        import scala.collection.JavaConverters._

        val targetResult = normalizeMatch(p.proc_, input)
        val cases = p.listcase_.asScala.toList.map {
          case ci: CaseImpl => (ci.proc_1, ci.proc_2)
          case _            => throw new Error("Unexpected Case implementation.")
        }

        val initAcc = (Seq[MatchCase](), targetResult.knownFree, BitSet())
        val casesResult = (initAcc /: cases) { (acc, caseImpl) =>
          caseImpl match {
            case (pattern, caseBody) => {
              val patternResult =
                normalizeMatch(
                  pattern,
                  ProcVisitInputs(Par(), DebruijnLevelMap[VarSort](), DebruijnLevelMap[VarSort]()))
              val caseEnv = input.env.absorbFree(patternResult.knownFree)._1
              val caseBodyResult =
                normalizeMatch(caseBody, ProcVisitInputs(Par(), caseEnv, acc._2))
              (Seq(MatchCase(patternResult.par, caseBodyResult.par)) ++ acc._1,
               caseBodyResult.knownFree,
               acc._3 | caseBodyResult.par.locallyFree)
            }
          }
        }
        val freeCount = casesResult._2.count - input.knownFree.count
        ProcVisitOutputs(
          input.par.prepend(
            Match(targetResult.par,
                  casesResult._1.reverse,
                  freeCount,
                  (casesResult._3 | targetResult.par.locallyFree).until(input.env.next))),
          casesResult._2
        )
      }

      case p: PIf     => normalizeIfElse(p.proc_1, p.proc_2, new PNil())
      case p: PIfElse => normalizeIfElse(p.proc_1, p.proc_2, p.proc_3)

      case _ => throw new Error("Compilation of construct not yet supported.")
    }
  }
}

// Parameterized over T, the kind of typing discipline we are enforcing.
class DebruijnLevelMap[T](val next: Int, val env: Map[String, (Int, T)], val wildcardCount: Int) {
  def this() = this(0, Map[String, (Int, T)](), 0)

  def newBinding(binding: (String, T)): (DebruijnLevelMap[T], Int) =
    binding match {
      case (varName, sort) =>
        (DebruijnLevelMap[T](next + 1, env + (varName -> ((next, sort))), wildcardCount), next)
    }

  // Returns the new map, and the first value assigned. Given that they're assigned contiguously
  def newBindings(bindings: List[(String, T)]): (DebruijnLevelMap[T], Int) = {
    val newMap = (this /: bindings)((map, binding) => map.newBinding(binding)._1)
    (newMap, next)
  }

  // Returns the new map, and a list of the shadowed variables
  def absorbFree(binders: DebruijnLevelMap[T]): (DebruijnLevelMap[T], List[String]) = {
    val finalNext          = next + binders.next
    val finalWildcardCount = wildcardCount + binders.wildcardCount
    val adjustNext         = next
    binders.env.foldLeft((this, List[String]())) {
      case ((db: DebruijnLevelMap[T], shadowed: List[String]),
            (k: String, (level: Int, varType: T @unchecked))) => {
        val shadowedNew = if (db.env.contains(k)) k :: shadowed else shadowed
        (DebruijnLevelMap(finalNext,
                          db.env + (k -> ((level + adjustNext, varType))),
                          finalWildcardCount),
         shadowedNew)
      }
    }
  }

  // Returns the new map
  def setWildcardUsed(count: Int): DebruijnLevelMap[T] =
    DebruijnLevelMap(next, env, wildcardCount + count)

  def getBinding(varName: String): Option[T] =
    for (pair <- env.get(varName)) yield pair._2
  def getLevel(varName: String): Option[Int] =
    for (pair <- env.get(varName)) yield pair._1
  def get(varName: String): Option[(Int, T)] = env.get(varName)
  def isEmpty()                              = next == 0

  def count: Int            = next + wildcardCount
  def countNoWildcards: Int = next

  override def equals(that: Any): Boolean =
    that match {
      case that: DebruijnLevelMap[T] =>
        next == that.next &&
          env == that.env
      case _ => false
    }

  override def hashCode(): Int =
    (next.hashCode() * 37 + env.hashCode)
}

object DebruijnLevelMap {
  def apply[T](next: Int, env: Map[String, (Int, T)], wildcardCount: Int): DebruijnLevelMap[T] =
    new DebruijnLevelMap(next, env, wildcardCount)

  def apply[T](): DebruijnLevelMap[T] = new DebruijnLevelMap[T]()

  def unapply[T](db: DebruijnLevelMap[T]): Option[(Int, Map[String, (Int, T)], Int)] =
    Some((db.next, db.env, db.wildcardCount))
}

case class ProcVisitInputs(par: Par,
                           env: DebruijnLevelMap[VarSort],
                           knownFree: DebruijnLevelMap[VarSort])
// Returns the update Par and an updated map of free variables.
case class ProcVisitOutputs(par: Par, knownFree: DebruijnLevelMap[VarSort])

case class NameVisitInputs(env: DebruijnLevelMap[VarSort], knownFree: DebruijnLevelMap[VarSort])
case class NameVisitOutputs(chan: Channel, knownFree: DebruijnLevelMap[VarSort])

case class CollectVisitInputs(env: DebruijnLevelMap[VarSort], knownFree: DebruijnLevelMap[VarSort])
case class CollectVisitOutputs(expr: Expr, knownFree: DebruijnLevelMap[VarSort])
