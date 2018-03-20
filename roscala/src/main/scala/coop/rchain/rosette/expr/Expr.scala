package coop.rchain.rosette.expr

import coop.rchain.rosette.Ob
import coop.rchain.rosette.Ob.SingletonOb

sealed trait Expr                extends Ob
case class LetExpr()             extends Expr with SingletonOb
case class Symbol(value: String) extends Expr with SingletonOb
case object RequestExpr          extends Expr with SingletonOb
