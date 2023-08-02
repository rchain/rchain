package coop.rchain.models.rholangn

import cats.Eval
import coop.rchain.catscontrib.effect.implicits.sEval
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.collection.immutable.Seq

class StackSafetySpec extends AnyFlatSpec with Matchers {

  def findMaxRecursionDepth(): Int = {
    def count(i: Int): Int =
      try {
        count(i + 1) //apparently, the try-catch is enough for tailrec to not work. Lucky!
      } catch {
        case _: StackOverflowError => i
      }
    println("About to find max recursion depth for this test run")
    val maxDepth = count(0)
    println(s"Calculated max recursion depth is $maxDepth")
    // Because of OOM errors on CI depth recursion is limited
    val maxDepthLimited = Math.min(200, maxDepth)
    println(s"Used recursion depth is limited to $maxDepthLimited")
    maxDepthLimited
  }

  "Rholang par" should "not blow up on a huge structure with List" in {
    import coop.rchain.models.Expr.ExprInstance.GInt
    import coop.rchain.models._
    import coop.rchain.models.rholang.implicits._
    import coop.rchain.models.serialization.implicits._
    import coop.rchain.shared.Serialize

    @tailrec
    def hugePar(n: Int, par: Par = Par(exprs = Seq(GInt(0)))): Par =
      if (n == 0) par
      else hugePar(n - 1, Par(exprs = Seq(EList(Seq(par)))))

    val maxRecursionDepth: Int = findMaxRecursionDepth()
    val par                    = hugePar(maxRecursionDepth)
    val anotherPar             = hugePar(maxRecursionDepth)

    noException shouldBe thrownBy {
      ProtoM.serializedSize(par).value

      val encoded = Serialize[Par].encode(par)
      Serialize[Par].decode(encoded)

      HashM[Par].hash[Eval](par).value
      par.hashCode()

      EqualM[Par].equal[Eval](par, anotherPar).value
      par == anotherPar
    }
  }
  "RholangN par" should "not blow up on a huge structure with List" in {

    @tailrec
    def hugePar(n: Int, par: ParN = GIntN(0)): ParN =
      if (n == 0) par
      else hugePar(n - 1, EListN(par))

    val maxRecursionDepth: Int = findMaxRecursionDepth()
    val par                    = hugePar(maxRecursionDepth)
    val anotherPar             = hugePar(maxRecursionDepth)
    noException shouldBe thrownBy {
      val sData   = par.toBytes
      val decoded = ParN.fromBytes(sData)
      assert(par == decoded)
      assert(par.rhoHash == anotherPar.rhoHash)
      assert(par.serializedSize == anotherPar.serializedSize)
      assert(par == anotherPar)
      par == anotherPar
    }
  }
}
