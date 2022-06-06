package coop.rchain.models

import java.util.Objects

import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.testImplicits._
import monix.eval.Coeval
import org.scalacheck.Arbitrary
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime

class HashMSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(sizeRange = 100, minSuccessful = 50)

  behavior of "HashM"

  sameResultAsReference[Par]
  sameResultAsReference[Expr]
  sameResultAsReference[BindPattern]
  sameResultAsReference[Bundle]
  sameResultAsReference[Connective]
  sameResultAsReference[ConnectiveBody]
  sameResultAsReference[EList]
  sameResultAsReference[EMap]
  sameResultAsReference[EMatches]
  sameResultAsReference[EMethod]
  sameResultAsReference[ENeq]
  sameResultAsReference[ENot]
  sameResultAsReference[EOr]
  sameResultAsReference[ESet]
  sameResultAsReference[ETuple]
  sameResultAsReference[EVar]
  sameResultAsReference[GUnforgeable]
  sameResultAsReference[GPrivate]
  sameResultAsReference[GDeployerId]
  sameResultAsReference[KeyValuePair]
  sameResultAsReference[ListBindPatterns]
  sameResultAsReference[Match]
  sameResultAsReference[MatchCase]
  sameResultAsReference[New]
  sameResultAsReference[ParWithRandom]
  sameResultAsReference[PCost]
  sameResultAsReference[Receive]
  sameResultAsReference[ReceiveBind]
  sameResultAsReference[Send]
  sameResultAsReference[TaggedContinuation]
  sameResultAsReference[Var]
  sameResultAsReference[VarRef]
  sameResultAsReference[ParSet]
  sameResultAsReference[ParMap]

  assertTypeError("sameResultAsReference[Long]") //see the private instance in HashM
  sameResultAsReference[String]
  sameResultAsReference[ByteString]
  sameResultAsReference[BitSet]
  sameResultAsReference[AlwaysEqual[BitSet]]

  sameResultAsReference[SortedParHashSet]
  sameResultAsReference[SortedParMap]

  //fixed regressions / corner cases:
  sameResultAsReference(GInt(-1))
  sameResultAsReference(Expr(GInt(-1)))

  def sameResultAsReference[A <: Any: HashM: Arbitrary: Pretty](implicit tag: ClassTag[A]): Unit =
    it must s"provide same results as hashCode for ${tag.runtimeClass.getSimpleName}" in {
      forAll { a: PrettyPrinted[A] =>
        sameResultAsReference(a.value)
      }
    }

  private def sameResultAsReference[A <: Any: HashM](a: A): Assertion = {
    // We are going to override the generated hashCode for our generated AST classes,
    // so in this test we rely on the underlying implementation from ScalaRuntime,
    // and hard-code the current definition for the handmade AST classes.
    val reference = a match {
      //FIXME this should delegate to reference
      case x: ParSet          => Objects.hash(x.ps, x.remainder, Boolean.box(x.connectiveUsed))
      case x: ParMap          => Objects.hash(x.ps, x.remainder, Boolean.box(x.connectiveUsed))
      case caseClass: Product => ScalaRunTime._hashCode(caseClass)
      case _                  => a.hashCode()
    }
    // With this check we know that:
    //   HashM.hash[Id] == _.hashCode == reference
    // This makes this test valid both before and after we override the generated hashCode
    // (which is long done when you're reading this).
    reference should be(a.hashCode())
    val result = HashM[A].hash[Coeval](a).value
    result should be(reference)
  }
}
