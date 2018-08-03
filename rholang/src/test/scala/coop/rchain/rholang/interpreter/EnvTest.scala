package coop.rchain.rholang.interpreter

import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable

import coop.rchain.models.rholang.implicits._
import coop.rchain.models.Par

class EnvSpec extends FlatSpec with Matchers {

  val source0: Par = GPrivateBuilder()
  val source1: Par = GPrivateBuilder()
  val source2: Par = GPrivateBuilder()
  val source3: Par = GPrivateBuilder()
  val source4: Par = GPrivateBuilder()

  "Data" should "always be inserted at the next available level index" in {
    val result: Env[Par] = Env().put(source0, source1, source2)
    result should be(Env[Par](Map(0 -> source0, 1 -> source1, 2 -> source2), 3, 0))
  }

  "that.Env indices" should "be incremented by level of this.Env indices" in {
    val target1: Env[Par] = Env.makeEnv(source0, source1, source2)
    val target2: Env[Par] = Env.makeEnv(source3, source4)
    val result            = target1.merge(target2)
    result should be(
      Env[Par](Map(0 -> source0, 1 -> source1, 2 -> source2, 3 -> source3, 4 -> source4), 5, 0))
  }
}
