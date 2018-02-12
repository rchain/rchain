package coop.rchain.rosette

package object prim {
  // For now keys in Prims correspond to Prim::primnum in Rosette
  val Prims = Map[Int, Prim](197 -> rblfloat.flPlus,
                             222 -> fixnum.fxMod,
                             223 -> fixnum.fxDiv,
                             224 -> fixnum.fxTimes,
                             225 -> fixnum.fxMinus,
                             226 -> fixnum.fxPlus)

}
