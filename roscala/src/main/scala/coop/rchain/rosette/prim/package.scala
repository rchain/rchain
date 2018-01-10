package coop.rchain.rosette

package object prim {
  // For now keys in Prims correspond to Prim::primnum in Rosette
  val Prims = Map[Int, Prim](197 -> RblFloat.flPlus,
                             222 -> Fixnum.fxMod,
                             223 -> Fixnum.fxDiv,
                             224 -> Fixnum.fxTimes,
                             225 -> Fixnum.fxMinus,
                             226 -> Fixnum.fxPlus)

}
