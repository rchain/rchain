package coop.rchain.rosette

import scala.collection.mutable

case class Prim(override val _slot: mutable.Seq[Ob]) extends Ob {
  def dispatchHelper(ctxt: Ctxt): Either[RblError, Ob] = Right(null)
}

object Prim {
  val nthPrim: Seq[Prim] = new Array[Prim](0)

  object INVALID extends Prim(null)

  def oprnMissingMethod: Ob = ???
}
