package coop.rchain.rholang.interpreter

import java.io.ByteArrayOutputStream
import java.util

import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance.{EEvalBody, GInt, GString}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._
import coop.rchain.models.testImplicits._
import coop.rchain.rholang.interpreter.implicits._
import org.scalactic.anyvals.PosInt
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class ESetSerializationTest extends FlatSpec with PropertyChecks with Matchers {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(1000))

  val pars: Seq[Par] = Seq(
    Send(Quote(GString("result")), List(GString("Success")), false, BitSet()),
    Receive(
      Seq(ReceiveBind(Seq(ChanVar(FreeVar(0))), Quote(GInt(1)), freeCount = 1)),
      EEvalBody(ChanVar(BoundVar(0))),
      false,
      1,
      BitSet()
    )
  )

  val locallyFree: BitSet = BitSet(3, 2, 1, 5)
  val set = ESet(pars, locallyFree, connectiveUsed = true)
  val os = new ByteArrayOutputStream()
  val refByteString = set.writeTo(os)
  val ba = os.toByteArray

  "ESet" should "be serialized deterministically" in {
    forAll { (_: Par) =>
      val tos = new ByteArrayOutputStream()
      set.writeTo(tos)
      val tba = tos.toByteArray
      util.Arrays.equals(ba, tba) should be(true)
    }
  }
}