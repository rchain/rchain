package coop.rchain.rholang.intepreter

import coop.rchain.rholang.syntax.rholang_mercury
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{Ground => AbsynGround, _}

sealed trait VarSort
case object ProcSort extends VarSort
case object NameSort extends VarSort

object BoolNormalizeMatcher {
  def normalizeMatch(b: Bool): GBool =
    b match {
      case b: BoolTrue  => GBool(true)
      case b: BoolFalse => GBool(false)
    }
}

object GroundNormalizeMatcher {
  def normalizeMatch(g: AbsynGround): Ground =
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
    def foldMatch(listproc: List[Proc], constructor: List[Par] => Expr): CollectVisitOutputs = {
      val folded = ((List[Par](), input.knownFree) /: listproc)(
        (acc, e) => {
          val result =
            ProcNormalizeMatcher.normalizeMatch(e, ProcVisitInputs(Par(), input.env, acc._2))
          (result.par :: acc._1, result.knownFree)
        }
      )
      CollectVisitOutputs(constructor(folded._1.reverse), folded._2)
    }

    def foldMatchMap(listproc: List[KeyValuePair],
                     constructor: List[(Par, Par)] => Expr): CollectVisitOutputs = {
      val folded = ((List[(Par, Par)](), input.knownFree) /: listproc)(
        (acc, e) => {
          e match {
            case e: KeyValuePairImpl => {
              val keyResult =
                ProcNormalizeMatcher.normalizeMatch(e.proc_1,
                                                    ProcVisitInputs(Par(), input.env, acc._2))
              val valResult = ProcNormalizeMatcher
                .normalizeMatch(e.proc_2, ProcVisitInputs(Par(), input.env, keyResult.knownFree))
              ((keyResult.par, valResult.par) :: acc._1, valResult.knownFree)
            }
          }
        }
      )
      CollectVisitOutputs(constructor(folded._1.reverse), folded._2)
    }
    c match {
      case cl: CollectList  => foldMatch(cl.listproc_.asScala.toList, EList)
      case ct: CollectTuple => foldMatch(ct.listproc_.asScala.toList, ETuple)
      case cs: CollectSet   => foldMatch(cs.listproc_.asScala.toList, ESet)
      case cm: CollectMap   => foldMatchMap(cm.listkeyvaluepair_.asScala.toList, EMap)
    }
  }
}

object NameNormalizeMatcher {
  def normalizeMatch(n: Name, input: NameVisitInputs): NameVisitOutputs =
    n match {
      case n: NameWildcard =>
        val wildcardBindResult = input.knownFree.setWildcardUsed(1)
        NameVisitOutputs(ChanVar(FreeVar(wildcardBindResult._2)), wildcardBindResult._1)
      case n: NameVar =>
        input.env.get(n.var_) match {
          case Some((level, NameSort)) => {
            NameVisitOutputs(ChanVar(BoundVar(level)), input.knownFree)
          }
          case Some((level, ProcSort)) => {
            throw new Error("Proc variable used in name context.")
          }
          case None => {
            input.knownFree.get(n.var_) match {
              case None =>
                val newBindingsPair =
                  input.knownFree.newBindings(List((Some(n.var_), NameSort)))
                NameVisitOutputs(ChanVar(FreeVar(newBindingsPair._2(0))), newBindingsPair._1)
              case _ => throw new Error("Free variable used as binder may not be used twice.")
            }
          }
        }

      case n: NameQuote => {
        def collapseQuoteEval(p: Par): Channel =
          p.singleEval() match {
            case Some(Eval(chanNew)) => chanNew
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
    def unaryExp(subProc: Proc,
                 input: ProcVisitInputs,
                 constructor: Par => Expr): ProcVisitOutputs = {
      val subResult = normalizeMatch(subProc, input.copy(par = Par()))
      ProcVisitOutputs(input.par.copy(exprs = constructor(subResult.par) :: input.par.exprs),
                       subResult.knownFree)
    }
    def binaryExp(subProcLeft: Proc,
                  subProcRight: Proc,
                  input: ProcVisitInputs,
                  constructor: (Par, Par) => Expr): ProcVisitOutputs = {
      val leftResult = normalizeMatch(subProcLeft, input.copy(par = Par()))
      val rightResult =
        normalizeMatch(subProcRight, input.copy(par = Par(), knownFree = leftResult.knownFree))
      ProcVisitOutputs(
        input.par.copy(exprs = constructor(leftResult.par, rightResult.par) :: input.par.exprs),
        rightResult.knownFree)
    }

    p match {
      case p: PGround =>
        ProcVisitOutputs(
          input.par.copy(
            exprs = GroundNormalizeMatcher.normalizeMatch(p.ground_) :: input.par.exprs),
          input.knownFree)

      case p: PCollect => {
        val collectResult = CollectionNormalizeMatcher.normalizeMatch(
          p.collection_,
          CollectVisitInputs(input.env, input.knownFree))
        ProcVisitOutputs(input.par.copy(exprs = collectResult.expr :: input.par.exprs),
                         collectResult.knownFree)
      }

      case p: PVar =>
        input.env.get(p.var_) match {
          case Some((level, ProcSort)) => {
            ProcVisitOutputs(input.par.copy(
                               exprs = EVar(BoundVar(level))
                                 :: input.par.exprs),
                             input.knownFree)
          }
          case Some((level, NameSort)) => {
            throw new Error("Name variable used in process context.")
          }
          case None => {
            input.knownFree.get(p.var_) match {
              case None =>
                val newBindingsPair =
                  input.knownFree.newBindings(List((Some(p.var_), ProcSort)))
                ProcVisitOutputs(input.par.copy(
                                   exprs = EVar(FreeVar(newBindingsPair._2(0)))
                                     :: input.par.exprs),
                                 newBindingsPair._1)
              case _ => throw new Error("Free variable used as binder may not be used twice.")
            }
          }
        }

      case p: PNil => ProcVisitOutputs(input.par, input.knownFree)

      case p: PEval => {
        def collapseEvalQuote(chan: Channel): Par =
          chan match {
            case Quote(p) => p
            case _        => Par().copy(evals = List(Eval(chan)))
          }

        val nameMatchResult =
          NameNormalizeMatcher.normalizeMatch(p.name_, NameVisitInputs(input.env, input.knownFree))
        ProcVisitOutputs(input.par.merge(collapseEvalQuote(nameMatchResult.chan)),
                         nameMatchResult.knownFree)
      }

      case p: PNot => unaryExp(p.proc_, input, ENot)
      case p: PNeg => unaryExp(p.proc_, input, ENeg)

      case p: PMult  => binaryExp(p.proc_1, p.proc_2, input, EMult)
      case p: PDiv   => binaryExp(p.proc_1, p.proc_2, input, EDiv)
      case p: PAdd   => binaryExp(p.proc_1, p.proc_2, input, EPlus)
      case p: PMinus => binaryExp(p.proc_1, p.proc_2, input, EMinus)

      case p: PLt  => binaryExp(p.proc_1, p.proc_2, input, ELt)
      case p: PLte => binaryExp(p.proc_1, p.proc_2, input, ELte)
      case p: PGt  => binaryExp(p.proc_1, p.proc_2, input, EGt)
      case p: PGte => binaryExp(p.proc_1, p.proc_2, input, EGte)

      case p: PEq  => binaryExp(p.proc_1, p.proc_2, input, EEq)
      case p: PNeq => binaryExp(p.proc_1, p.proc_2, input, ENeq)

      case p: PAnd => binaryExp(p.proc_1, p.proc_2, input, EAnd)
      case p: POr  => binaryExp(p.proc_1, p.proc_2, input, EOr)

      case p: PSend => {
        import scala.collection.JavaConverters._
        val nameMatchResult =
          NameNormalizeMatcher.normalizeMatch(p.name_, NameVisitInputs(input.env, input.knownFree))
        val initAcc = (List[Par](), ProcVisitInputs(Par(), input.env, nameMatchResult.knownFree))
        val dataResults = (initAcc /: p.listproc_.asScala.toList.reverse)(
          (acc, e) => {
            val procMatchResult = normalizeMatch(e, acc._2)
            (procMatchResult.par :: acc._1,
             ProcVisitInputs(Par(), input.env, procMatchResult.knownFree))
          }
        )
        val persistent = p.send_ match {
          case _: SendSingle   => false
          case _: SendMultiple => true
        }
        ProcVisitOutputs(
          input.par.copy(
            sends = Send(nameMatchResult.chan, dataResults._1, persistent) :: input.par.sends),
          dataResults._2.knownFree)
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
        val newEnv       = input.env.absorbFree(formalsResults._2)
        val newFreeCount = formalsResults._2.next
        val bodyResult = ProcNormalizeMatcher.normalizeMatch(
          p.proc_,
          ProcVisitInputs(Par(), newEnv, nameMatchResult.knownFree))
        ProcVisitOutputs(
          input.par.copy(
            receives = Receive(List((formalsResults._1.reverse, nameMatchResult.chan)),
                               bodyResult.par,
                               true,
                               newFreeCount) :: input.par.receives),
          bodyResult.knownFree
        )
      }

      case p: PPar => {
        val result       = normalizeMatch(p.proc_1, input)
        val chainedInput = input.copy(knownFree = result.knownFree, par = result.par)
        normalizeMatch(p.proc_2, chainedInput)
      }

      case _ => throw new Error("Compilation of construct not yet supported.")
    }
  }
}

// Parameterized over T, the kind of typing discipline we are enforcing.
class DebruijnLevelMap[T](val next: Int, val env: Map[String, (Int, T)]) {
  def this() = this(0, Map[String, (Int, T)]())

  def newBindings(bindings: List[(Option[String], T)]): (DebruijnLevelMap[T], List[Int]) = {
    val result = bindings.foldLeft((this, List[Int]())) {
      (acc: (DebruijnLevelMap[T], List[Int]), binding: (Option[String], T)) =>
        {
          val newMap = binding._1 match {
            case None          => acc._1.env
            case Some(varName) => acc._1.env + (varName -> ((acc._1.next, binding._2)))
          }
          (DebruijnLevelMap(acc._1.next + 1, newMap), acc._1.next :: acc._2)
        }
    }
    (result._1, result._2.reverse)
  }

  def absorbFree(binders: DebruijnLevelMap[T]): DebruijnLevelMap[T] = {
    val finalNext  = next + binders.next
    val adjustNext = next
    binders.env.foldLeft(this) {
      case (db: DebruijnLevelMap[T], (k: String, (level: Int, varType: T @unchecked))) =>
        DebruijnLevelMap(finalNext, db.env + (k -> ((level + adjustNext, varType))))
    }
  }

  // Returns the new map, and the starting level of the newly "bound" wildcards
  def setWildcardUsed(count: Int): (DebruijnLevelMap[T], Int) =
    (DebruijnLevelMap(next + count, env), next)

  def getBinding(varName: String): Option[T] =
    for (pair <- env.get(varName)) yield pair._2
  def getLevel(varName: String): Option[Int] =
    for (pair <- env.get(varName)) yield pair._1
  def get(varName: String): Option[(Int, T)] = env.get(varName)
  def isEmpty()                              = next == 0

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
  def apply[T](next: Int, env: Map[String, (Int, T)]): DebruijnLevelMap[T] =
    new DebruijnLevelMap(next, env)

  def apply[T](): DebruijnLevelMap[T] = new DebruijnLevelMap[T]()

  def unapply[T](db: DebruijnLevelMap[T]): Option[(Int, Map[String, (Int, T)])] =
    Some((db.next, db.env))
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
