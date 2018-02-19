package coop.rchain.models

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class RhoTypesTest extends PropSpec with PropertyChecks with Matchers {
  def rtt[T](msg: T, serializeInstance: Serialize[T]): Unit = {
    val bytes = serializeInstance.encode(msg)
    val msg2  = serializeInstance.decode(bytes)
    assert(msg2.isRight && msg == msg2.right.get)

    val errBytes = Array[Byte](1, 2, 3, 4)
    val msg3     = serializeInstance.decode(errBytes)
    assert(msg3.isLeft)
  }

  property("complex value should pass round-trip serialization") {
    val lenVariants = for (n <- Gen.choose(0, 50)) yield n
    forAll(lenVariants) { (len: Int) =>
      forAll { (p1: Int, p2: Int) =>
        val n1 = New().withCount(p1)
        val n2 = New().withCount(p2)

        val par1 = Par().withNews(List.fill(len)(n1))
        val par2 = Par().withNews(List.fill(len)(n2))
        val kv   = ParTuple().withP1(par1).withP2(par2)

        val emap = EMap().withKvs(List.fill(len)(kv))
        val expr = Expr().withEMap(emap)

        val outerPar = Par().withExprs(List(expr))

        rtt(outerPar, parInstance)
      }
    }
  }
}
