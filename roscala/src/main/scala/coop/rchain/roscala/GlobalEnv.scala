package coop.rchain.roscala

import coop.rchain.roscala.ob.Ob

import scala.collection.mutable

class GlobalEnv {
  val keys   = mutable.ArrayBuffer[Ob]()
  val values = mutable.ArrayBuffer[Ob]()
}
