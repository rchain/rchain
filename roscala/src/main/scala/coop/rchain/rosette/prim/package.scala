package coop.rchain.rosette

package object prim {
  // For now keys in Prims correspond to Prim::primnum in Rosette
  val Prims = Map[Int, Prim](222 -> Number.fxMod,
                             223 -> Number.fxDiv,
                             224 -> Number.fxTimes,
                             225 -> Number.fxMinus,
                             226 -> Number.fxPlus)

}
