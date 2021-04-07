package coop.rchain.rholang.interpreter

import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.debugger.DebugInfo
import org.scalatest.{FlatSpec, Matchers}

class EnvSpec extends FlatSpec with Matchers {

  val source0: Par = GPrivateBuilder()
  val source1: Par = GPrivateBuilder()
  val source2: Par = GPrivateBuilder()
  val source3: Par = GPrivateBuilder()
  val source4: Par = GPrivateBuilder()

  "Data" should "always be inserted at the next available level index" in {
    val result: Env[Par] = Env().put(source0).put(source1).put(source2)
    result should be(
      Env[Par](Map(0 -> source0, 1 -> source1, 2 -> source2), 3, 0, DebugInfo())
    )
  }
}
