package coop.rchain.rholang.intepreter

import coop.rchain.rholang.syntax.rholang_mercury
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{Ground => AbsynGround, _}

sealed trait VarSort
case object ProcSort extends VarSort
case object NameSort extends VarSort

trait BoolNormalizeVisitor extends Bool.Visitor[GBool, Any] {
  override def visit( b: BoolTrue, n: Any ) : GBool = GBool(true)
  override def visit( b: BoolFalse, n: Any ) : GBool = GBool(false)
}

trait GroundNormalizeVisitor extends AbsynGround.Visitor[Ground, Any] 
    with BoolNormalizeVisitor {
  override def visit( gb: GroundBool, n: Any ) : Ground = gb.bool_.accept(this, n)
  override def visit( gi: GroundInt, n: Any ) : Ground = GInt(gi.integer_)
  override def visit( gi: GroundString, n: Any ) : Ground = GString(gi.string_)
  override def visit( gi: GroundUri, n: Any ) : Ground = GUri(gi.uri_)
}

trait NameNormalizeVisitor extends Name.Visitor[NameVisitOutputs, NameVisitInputs] {
  val procVisitor : Proc.Visitor[ProcVisitOutputs, ProcVisitInputs]

  override def visit( n: NameWildcard, input: NameVisitInputs ) = {
    NameVisitOutputs(ChanVar(WildCard()), input.knownFree.setWildcardUsed())
  }
  override def visit( n: NameVar, input: NameVisitInputs ) = {
    input.env.get(n.var_) match {
      case Some( (level, NameSort) ) => {
        NameVisitOutputs(
          ChanVar(BoundVar(level)),
          input.knownFree)
      }
      case Some( (level, ProcSort) ) => {
        throw new Error("Proc variable used in process context.")
      }
      case None => {
        input.knownFree.get(n.var_) match {
          case None =>
            val newBindingsPair = 
              input.knownFree.newBindings(List((n.var_, NameSort)))
            NameVisitOutputs(
              ChanVar(FreeVar(newBindingsPair._2(0))),
              newBindingsPair._1)
          case _ => throw new Error(
            "Free variable used as binder may not be used twice.")
        }
      }
    }
  }

  override def visit( n: NameQuote, input: NameVisitInputs ) = {
    val procVisitResult : ProcVisitOutputs = n.proc_.accept(
        procVisitor,
        ProcVisitInputs(Par(), input.env, input.knownFree))
    NameVisitOutputs(Quote(procVisitResult.par),
      procVisitResult.knownFree)
  }
}

trait ProcNormalizeVisitor
    extends Proc.Visitor[ProcVisitOutputs, ProcVisitInputs]
    with GroundNormalizeVisitor {

  override def visit( p: PGround, input: ProcVisitInputs )
      : ProcVisitOutputs = {
    ProcVisitOutputs(
      input.par.copy(expr = p.ground_.accept(this, input) :: input.par.expr),
      input.knownFree)
  }

  override def visit( p: PVar, input: ProcVisitInputs )
      : ProcVisitOutputs = {
    input.env.get(p.var_) match {
      case Some( (level, ProcSort) ) => {
        ProcVisitOutputs(
          input.par.copy(expr = EVar(BoundVar(level))
                         :: input.par.expr),
          input.knownFree)
      }
      case Some( (level, NameSort) ) => {
        throw new Error("Name variable used in process context.")
      }
      case None => {
        input.knownFree.get(p.var_) match {
          case None =>
            val newBindingsPair = 
              input.knownFree.newBindings(List((p.var_, ProcSort)))
            ProcVisitOutputs(
              input.par.copy(expr = EVar(FreeVar(newBindingsPair._2(0)))
                             :: input.par.expr),
              newBindingsPair._1)
          case _ => throw new Error(
            "Free variable used as binder may not be used twice.")
        }
      }
    }
  }

  override def visit( p: PNil, input: ProcVisitInputs ) : ProcVisitOutputs = {
    ProcVisitOutputs(input.par, input.knownFree)
  }

  override def visit( p: PPar, input: ProcVisitInputs ) : ProcVisitOutputs = {
    // Binders are numbered in lexicographical order.
    val result = p.proc_1.accept(this, input)
    val chainedInput = input.copy(
      knownFree = result.knownFree,
      par = result.par)
    p.proc_2.accept(this, chainedInput)
  }
    
  override def visit( p: PCollect, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PEval, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PMethod, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PNot, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PNeg, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PMult, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PDiv, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PAdd, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PMinus, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PLt, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PLte, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PGt, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PGte, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PEq, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PMatches, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PNeq, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PAnd, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: POr, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PSend, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PContr, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PInput, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PChoice, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PMatch, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PIf, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PIfElse, input: ProcVisitInputs ) : ProcVisitOutputs = ???
  override def visit( p: PNew, input: ProcVisitInputs ) : ProcVisitOutputs = ???
}


// Parameterized over T, the kind of typing discipline we are enforcing.
class DebruijnLevelMap[T](val next: Int, val env: Map[String, (Int, T)], val wildcardUsed: Boolean) {
  def this() = this(0, Map[String, (Int, T)](), false)

  def newBindings(bindings: List[(String, T)]): (DebruijnLevelMap[T], List[Int]) = {
    val result = bindings.foldLeft((this, List[Int]())) {
      (acc: (DebruijnLevelMap[T], List[Int]), binding: (String,T)) =>
      (DebruijnLevelMap(
          acc._1.next + 1,
          acc._1.env + (binding._1 -> ((acc._1.next, binding._2))),
          wildcardUsed),
       acc._1.next :: acc._2)
    }
    (result._1, result._2.reverse)
  }

  def setWildcardUsed() : DebruijnLevelMap[T] = {
    if (wildcardUsed) {
      this
    } else {
      DebruijnLevelMap(next, env, true)
    }
  }

  def getBinding(varName: String) : Option[T] = {
    for (pair <- env.get(varName)) yield pair._2
  }
  def getLevel(varName: String) : Option[Int] = {
    for (pair <- env.get(varName)) yield pair._1
  }
  def get(varName: String) : Option [(Int, T)] = env.get(varName)
  def isEmpty() = env.isEmpty

  override def equals(that: Any) = {
    that match {
      case that: DebruijnLevelMap[T] =>
        next == that.next &&
        env == that.env &&
        wildcardUsed == that.wildcardUsed
      case _ => false
    }
  }

  override def hashCode() = {
    (next.hashCode() * 37 + env.hashCode) * 37 + wildcardUsed.hashCode
  }
}

object DebruijnLevelMap{
  def apply[T](next: Int, env: Map[String, (Int, T)], wildcardUsed: Boolean)
      : DebruijnLevelMap[T] = {
    new DebruijnLevelMap(next, env, wildcardUsed)
  }

  def apply[T]() = new DebruijnLevelMap[T]()

  def unapply[T](db: DebruijnLevelMap[T]): Option[(Int, Map[String,(Int, T)], Boolean)] = {
    Some((db.next, db.env, db.wildcardUsed))
  }
}

case class ProcVisitInputs(
  par: Par,
  env: DebruijnLevelMap[VarSort],
  knownFree: DebruijnLevelMap[VarSort] )
// Returns the update Par and an updated map of free variables.
case class ProcVisitOutputs( par: Par, knownFree: DebruijnLevelMap[VarSort] )

sealed trait ChanPosition
case object BindingPosition extends ChanPosition
case object UsePosition extends ChanPosition

case class NameVisitInputs(
  env: DebruijnLevelMap[VarSort],
  knownFree: DebruijnLevelMap[VarSort] )
case class NameVisitOutputs( chan: Channel, knownFree: DebruijnLevelMap[VarSort] )
