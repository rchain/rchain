package coop.rchain.rholang.interpreter

import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable

import implicits._
import coop.rchain.models.Par

class EnvSpec extends FlatSpec with Matchers {

  val source0: Par = GPrivate()
  val source1: Par = GPrivate()
  val source2: Par = GPrivate()
  val source3: Par = GPrivate()
  val source4: Par = GPrivate()

  "Data" should "always be inserted at the next available level index" in {
    val result: Env[Par] = Env()
    result.put(source0, source1, source2)
    result should be(
      Env[Par](mutable.LinkedHashMap(0 -> source0, 1 -> source1, 2 -> source2), 3, 0))
  }

  "Level indices" should "be incremented by value of rename argument" in {
    val srcMap: Env[Par] = Env()
    srcMap.put(source0, source1, source2)
    val result = srcMap.rename(1)
    result should be(
      Env[Par](mutable.LinkedHashMap(1 -> source0, 2 -> source1, 3 -> source2), 4, 0))
  }

  "that.Env indices" should "be incremented by level of this.Env indices" in {
    val target1: Env[Par] = Env()
    target1.put(source0, source1, source2)
    val target2: Env[Par] = Env()
    target2.put(source3, source4)
    val result = target1
    target1.merge(target2)
    result should be(
      Env[Par](
        mutable.LinkedHashMap(0 -> source0, 1 -> source1, 2 -> source2, 3 -> source3, 4 -> source4),
        5,
        0))
  }
}
