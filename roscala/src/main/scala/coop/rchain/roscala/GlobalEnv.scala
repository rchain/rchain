package coop.rchain.roscala

import coop.rchain.roscala.ob.{Ob, TblObject}

class GlobalEnv extends TblObject {
  def values(n: Int): Ob = extension.slot(n)
}
