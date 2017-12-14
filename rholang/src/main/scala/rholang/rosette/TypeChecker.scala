// -*- mode: Scala;-*- 
// Filename:    TypeChecker.scala 
// Authors:     Kyle Butt, Eitan Chatav                                                  
// Creation:    Thurs Dec 7 11:43:00 2017 
// Copyright:   See site license 
// Description: Spatial type checker; see Namespace Logic paper by LGM.
// ------------------------------------------------------------------------

package coop.rchain.rho2rose

import coop.rchain.syntax.rholang._
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
    def nominallySafisfies( ind: TPIndicator, chan: Chan): Boolean = sys.error("unimplemented")
}
