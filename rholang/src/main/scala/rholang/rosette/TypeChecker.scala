// -*- mode: Scala;-*- 
// Filename:    TypeChecker.scala 
// Authors:     Kyle Butt, Eitan Chatav                                                  
// Creation:    Thurs Dec 7 11:43:00 2017 
// Copyright:   See site license 
// Description: Spatial type checker; see Namespace Logic paper by LGM.
// ------------------------------------------------------------------------

package coop.rchain.rho2rose

// import coop.rchain.syntax.rholang._
import coop.rchain.syntax.rholang.Absyn._

object CheckerTypes {
    type TypeError = String
    type Arg = Option[TPattern]
    type Ret = List[TypeError]
}

class TypeCheckVisitor
extends Contr.Visitor[CheckerTypes.Ret, CheckerTypes.Arg]
with Proc.Visitor[CheckerTypes.Ret, CheckerTypes.Arg]{
    import CheckerTypes._
    override def visit( p: DContr, arg: Arg ): Ret = {
        println("typechecker ran on" + p)
        List()
    }
    override def visit( p: PNil, arg: Arg ): Ret = List()
    override def visit( p: PValue, arg: Arg ): Ret = List()
    override def visit( p: PDrop, arg: Arg ): Ret = List()
    override def visit( p: PInject, arg: Arg ): Ret = List()
    override def visit( p: PLift, arg: Arg ): Ret = List()
    override def visit( p: PFoldL, arg: Arg ): Ret = List()
    override def visit( p: PFoldR, arg: Arg ): Ret = List()
    override def visit( p: PInput, arg: Arg ): Ret = List()
    override def visit( p: PChoice, arg: Arg ): Ret = List()
    override def visit( p: PMatch, arg: Arg ): Ret = List()
    override def visit( p: PNew, arg: Arg ): Ret = List()
    override def visit( p: PPrint, arg: Arg ): Ret = List()
    override def visit( p: PConstr, arg: Arg ): Ret = List()
    override def visit( p: PContr, arg: Arg ): Ret = List()
    override def visit( p: PPar, arg: Arg ): Ret = List()
}

class SatisfiedVisitor
extends TPattern.Visitor[Boolean, Proc]{
    override def visit( tp: TPVerity, arg: Proc ): Boolean = true
    override def visit( tp: TPNegation, arg: Proc ): Boolean = {
        ! (tp.tpattern_.accept(this, arg))
    }
    override def visit ( tp: TPNullity, arg: Proc): Boolean = {
      structurallyEquivalent(arg, new PNil())
    }
    override def visit ( tp: TPConjuction, arg: Proc): Boolean = {
      (tp.tpattern_1.accept(this, arg) && tp.tpattern_2.accept(this, arg))
    }
    override def visit ( tp: TPDisjunction, arg: Proc): Boolean = {
      (tp.tpattern_1.accept(this, arg) || tp.tpattern_2.accept(this, arg))
    }
    override def visit ( tp: TPDescent, arg: Proc): Boolean = {
      arg match {
        case pdrop: PDrop => nameEquivalent(pdrop.chan_, tp.chan_)
        case _: Any => false
      }
    }
    override def visit ( tp: TPElevation, arg: Proc): Boolean = {
      arg match {
        case plift : PLift => {
          if (plift.listproc_.size() == 1) {
            nominallySafisfies( tp.tpindicator_, plift.chan_ ) &&
            tp.tpattern_.accept(this, plift.listproc_.get(0))
          } else {
            sys.error("unimplemented")
          }
        }
        case _: Any => false
      }
    }
    override def visit ( tp: TPActivity, arg: Proc): Boolean = sys.error("unimplemented")
    override def visit ( tp: TPMixture, arg: Proc): Boolean = sys.error("unimplemented")
    
    def structurallyEquivalent( p1: Proc, p2: Proc): Boolean = sys.error("unimplemented")
    def nameEquivalent( p1: Chan, p2: Chan): Boolean = sys.error("unimplemented")
    def nominallySafisfies( ind: TPIndicator, chan: Chan): Boolean = {
      chan.accept(new NominallySatisfiedVisitor, ind)
    }
}

class NominallySatisfiedVisitor
extends Chan.Visitor[Boolean,TPIndicator]{
  override def visit ( cquote: CQuote, arg: TPIndicator): Boolean = {
    val proc = cquote.proc_
    arg match {
      case tpiquote: TPIQuotFormula =>
        tpiquote.tpattern_.accept(new SatisfiedVisitor, proc)
      case tpichan: TPIChan =>
        sys.error("unimplemented")
    }
  }
  override def visit ( cvar: CVar, arg: TPIndicator): Boolean = {
    sys.error("unimplemented")
  }
}

object Equivalences{

  def nameEquivalent(n1: Chan, n2: Chan): Boolean = {
    (n1, n2) match {
      case (q1: CQuote, q2: CQuote) =>
        q1.proc_ match {
          case pdrop1: PDrop => nameEquivalent(pdrop1.chan_, n2)
          case _ => q2.proc_ match {
            case pdrop2: PDrop => nameEquivalent(n1, pdrop2.chan_)
            case _ => structurallyEquivalent(q1.proc_, q2.proc_)
          }
        }
      case _ => false // TODO: variable equality requires tracking variable environment
    }
  }

  def structurallyNil(p: Proc): Boolean = {
    p match {
      case _ : PNil => true
      case ppar : PPar =>
        structurallyNil(ppar.proc_1) && structurallyNil (ppar.proc_2)
      case _ => false
    }
  }

  def structurallyEquivalent(p1: Proc, p2: Proc): Boolean = {
    (p1,p2) match {
      case (_ : PNil, _) => structurallyNil(p2)
      case (_, _ : PNil) => structurallyNil(p1)
      // case () => {
      //   nameEquivalent(nsubj1, nsubj2)
      //   && structurallyNil()
      // }
      case (_,_) => sys.error("unimplemented")
    }
  }
}

class DeBruijn(val environment: Map[String,Int], val next: Int){

  def this() = this(Map(), 1)

  def newBindings(bindings: List[String]): DeBruijn = {
    bindings.foldLeft(this) {
      (db: DeBruijn,str: String) =>
      DeBruijn(db.environment + (str -> db.next), db.next + 1)
    }
  }

}

object DeBruijn{

  def apply(environment: Map[String,Int], next: Int): DeBruijn = {
    new DeBruijn(environment, next)
  }

  def unapply(db: DeBruijn): Option[(Map[String,Int],Int)] = {
    Some((db.environment, db.next))
  }
  
}
