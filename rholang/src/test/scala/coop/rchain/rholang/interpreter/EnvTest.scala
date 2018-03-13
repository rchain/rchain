package coop.rchain.rholang.interpreter

import org.scalatest.{FlatSpec, Matchers}
import implicits._
import Env._
import coop.rchain.models.Par

class EnvSpec extends FlatSpec with Matchers {

  val source0: Par = GPrivate()
  val source1: Par = GPrivate()
  val source2: Par = GPrivate()
  val source3: Par = GPrivate()
  val source4: Par = GPrivate()

  "Data" should "always be inserted at the next available level index" in {
    val result: Env[Par] = Env(source0, source1, source2)
    result should be(Env[Par](0 -> source0, 1 -> source1, 2 -> source2))
  }

  "Level indices" should "be incremented by value of rename argument" in {
    val result: Env[Par] = Env(source0, source1, source2) rename 1
    result should be(Env[Par](1 -> source0, 2 -> source1, 3 -> source2))
  }

  "that.Env indices" should "be incremented by level of this.Env indices" in {
    val target1: Env[Par] = Env(source0, source1, source2)
    val target2: Env[Par] = Env(source3, source4)
    val result: Env[Par]  = target1 merge target2
    result should be(Env[Par](0 -> source0, 1 -> source1, 2 -> source2, 3 -> source3, 4 -> source4))
  }
}
