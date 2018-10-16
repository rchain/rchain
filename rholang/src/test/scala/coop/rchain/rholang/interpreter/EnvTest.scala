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
    val result: Env[Par] = Env().put(source0).put(source1).put(source2)
    result should be(Env[Par](Map(0 -> source0, 1 -> source1, 2 -> source2), 3, 0))
  }
}
