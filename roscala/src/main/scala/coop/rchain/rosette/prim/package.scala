package coop.rchain.rosette

package object prim {
  // For now keys in Prims correspond to Prim::primnum in Rosette
  // TODO:
  val Prims = Map[Int, Prim](226 -> Number.fxPlus,
                             1 -> Number.fxDiv,
                             2 -> Number.fxMinus,
                             3 -> Number.fxTimes,
                             4 -> Number.fxMod)

}
