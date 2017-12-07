package coop.rchain.rosette.expr

import coop.rchain.rosette.Slot

case class LetExpr(override val slot: Slot = Slot.Placeholder) extends Expr
