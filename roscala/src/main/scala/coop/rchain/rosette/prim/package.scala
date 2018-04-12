package coop.rchain.rosette

import cats.data.State

package object prim {
  type PrimResult = Either[PrimError, Ob]

  // For now keys in Prims correspond to Prim::primnum in Rosette
  val Prims = Map[Int, Prim](197 -> rblfloat.flPlus,
                             222 -> fixnum.fxMod,
                             223 -> fixnum.fxDiv,
                             224 -> fixnum.fxTimes,
                             225 -> fixnum.fxMinus,
                             226 -> fixnum.fxPlus)

}
