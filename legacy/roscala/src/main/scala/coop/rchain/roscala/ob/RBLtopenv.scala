package coop.rchain.roscala.ob
import java.util.concurrent.atomic.AtomicInteger

import coop.rchain.roscala.GlobalEnv

class RBLtopenv extends Ob {
  override def lookup(key: Ob, globalEnv: GlobalEnv): Ob = Absent
}

object RBLtopenv {
  def apply(): RBLtopenv = {
    /*
     * This constructor expects to be called *after* sufficient
     * initialization has been performed that it can safely allocate a
     * new StdMeta object.
     */
    val m         = new Meta(new AtomicInteger(1), false)
    val rBLtopenv = new RBLtopenv
    rBLtopenv.meta = m
    rBLtopenv.parent = Invalid
    rBLtopenv
  }
}
