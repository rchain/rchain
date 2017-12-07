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
