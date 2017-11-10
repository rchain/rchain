package coop.rchain.rosette.utils

import coop.rchain.rosette.{Ob, TblObject}
import shapeless._
import record._
import syntax.singleton._

import cats.Eq

import scala.collection.mutable

object Instances {

  //TODO possibly we can make a generic derivation for any T <: Ob
  implicit val lgenTbl = new LabelledGeneric[TblObject] {

    type Repr =
      Record.`'slot -> mutable.Seq[Ob], 'entry -> Seq[Ob], '_slot -> mutable.Seq[Ob]`.T

    def to(a: TblObject): Repr =
      ('slot ->> a.slot) :: ('entry ->> a.entry) :: ('_slot ->> a._slot) :: HNil

    def from(r: Repr): TblObject = {
      val slot = r('slot)
      val _slot = r('_slot)
      val entry = r('entry)
      val newSlot = _slot(0) +: _slot(1) +: slot
      TblObject(_slot = newSlot, entry = entry)
    }
  }

  implicit val eqOb = new Eq[Ob] {
    override def eqv(x: Ob, y: Ob) =
      x equals y
  }
}
