package coop.rchain.interpreter

import coop.rchain.syntax.rholang_mercury
import coop.rchain.syntax.rholang_mercury.Absyn.{Ground => AbsynGround, _}

import coop.rchain.interpreter._

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

trait ProcNormalizeVisitor
    extends Proc.Visitor[ProcVisitOutputs, ProcVisitInputs]
    with GroundNormalizeVisitor {

  override def visit(p: PGround, input: ProcVisitInputs)
      : ProcVisitOutputs = {
    ProcVisitOutputs(
      input.par.copy(expr = p.ground_.accept(this, input) :: input.par.expr),
      input.knownFree)
  }

  override def visit(p: PVar, input: ProcVisitInputs)
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

  override def visit(p: PNil, input: ProcVisitInputs) : ProcVisitOutputs = {
    ProcVisitOutputs(input.par, input.knownFree)
  }
    
  override def visit(p: PCollect, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PEval, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PMethod, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PNot, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PNeg, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PMult, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PDiv, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PAdd, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PMinus, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PLt, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PLte, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PGt, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PGte, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PEq, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PMatches, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PNeq, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PAnd, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: POr, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PSend, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PContr, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PInput, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PChoice, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PMatch, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PIf, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PIfElse, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PNew, input: ProcVisitInputs) : ProcVisitOutputs = ???
  override def visit(p: PPar, input: ProcVisitInputs) : ProcVisitOutputs = ???
}


// Parameterized over T, the kind of typing discipline we are enforcing.
class DebruijnLevelMap[T]( val next: Int, val env: Map[String, (Int, T)]) {
  def this() = this(0, Map[String, (Int, T)]())

  def newBindings(bindings: List[(String, T)]): (DebruijnLevelMap[T], List[Int]) = {
    val result = bindings.foldLeft((this, List[Int]())) {
      (acc: (DebruijnLevelMap[T], List[Int]), binding: (String,T)) =>
      (DebruijnLevelMap(
          acc._1.next + 1,
          acc._1.env + (binding._1 -> ((acc._1.next, binding._2)))),
       acc._1.next :: acc._2)
    }
    (result._1, result._2.reverse)
  }

  def getBinding(varName: String) : Option[T] = {
    for (pair <- env.get(varName)) yield pair._2
  }
  def getLevel(varName: String) : Option[Int] = {
    for (pair <- env.get(varName)) yield pair._1
  }
  def get(varName: String) : Option [(Int, T)] = env.get(varName)
  def isEmpty() = env.isEmpty
}

object DebruijnLevelMap{
  def apply[T](next: Int, env: Map[String, (Int, T)])
      : DebruijnLevelMap[T] = {
    new DebruijnLevelMap(next, env)
  }

  def apply[T]() = new DebruijnLevelMap[T]()

  def unapply[T](db: DebruijnLevelMap[T]): Option[(Int, Map[String,(Int, T)])] = {
    Some((db.next, db.env))
  }
}

case class ProcVisitInputs(
  par: Par,
  env: DebruijnLevelMap[VarSort],
  knownFree: DebruijnLevelMap[VarSort] )
// Returns the update Par and an updated map of free variables.
case class ProcVisitOutputs( par: Par, knownFree: DebruijnLevelMap[VarSort] )
