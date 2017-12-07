package coop.rchain.rosette
import coop.rchain.rosette.expr.Expr

case class Symbol(value: String, override val slot: Slot = Slot.Placeholder)
    extends Expr
