//package coop.rchain.models
//
//import com.google.protobuf.ByteString
//import coop.rchain.models.Expr.ExprInstance.GInt
//import coop.rchain.models.testImplicits._
//import coop.rchain.models.testUtils.TestUtils.forAllSimilarA
//import monix.eval.Coeval
//import org.scalacheck.{Arbitrary, Shrink}
//import org.scalatest.prop.PropertyChecks
//import org.scalatest.{Assertion, FlatSpec, Matchers}
//
//import scala.Function.tupled
//import scala.collection.immutable.BitSet
//import scala.reflect.ClassTag
//
//class EqualMSpec extends FlatSpec with PropertyChecks with Matchers {
//
//  implicit override val generatorDrivenConfig =
//    PropertyCheckConfiguration(sizeRange = 25, minSuccessful = 100)
//
//  behavior of "EqualM"
//
//  sameResultAsReference[Par]
//  sameResultAsReference[Expr]
//  sameResultAsReference[BindPattern]
//  sameResultAsReference[Bundle]
//  sameResultAsReference[Connective]
//  sameResultAsReference[ConnectiveBody]
//  sameResultAsReference[EList]
//  sameResultAsReference[EMap]
//  sameResultAsReference[EMatches]
//  sameResultAsReference[EMethod]
//  sameResultAsReference[ENeq]
//  sameResultAsReference[ENot]
//  sameResultAsReference[EOr]
//  sameResultAsReference[ESet]
//  sameResultAsReference[ETuple]
//  sameResultAsReference[EVar]
//  sameResultAsReference[GPrivate]
//  sameResultAsReference[KeyValuePair]
//  sameResultAsReference[ListBindPatterns]
//  sameResultAsReference[Match]
//  sameResultAsReference[MatchCase]
//  sameResultAsReference[New]
//  sameResultAsReference[ParWithRandom]
//  sameResultAsReference[PCost]
//  sameResultAsReference[Receive]
//  sameResultAsReference[ReceiveBind]
//  sameResultAsReference[Send]
//  sameResultAsReference[TaggedContinuation]
//  sameResultAsReference[Var]
//  sameResultAsReference[VarRef]
//  sameResultAsReference[ParSet]
//  sameResultAsReference[ParMap]
//
//  sameResultAsReference[Int]
//  sameResultAsReference[Long]
//  sameResultAsReference[String]
//  sameResultAsReference[ByteString]
//  sameResultAsReference[BitSet]
//  sameResultAsReference[AlwaysEqual[BitSet]]
//
//  sameResultAsReference[SortedParHashSet]
//  sameResultAsReference[SortedParMap]
//
//  //fixed regressions / corner cases:
//  sameResultAsReference(GInt(-1), GInt(-1))
//  sameResultAsReference(Expr(GInt(-1)), Expr(GInt(-1)))
//
//  def sameResultAsReference[A <: Any: EqualM: Arbitrary: Shrink: Pretty](
//      implicit tag: ClassTag[A]
//  ): Unit =
//    it must s"provide same results as equals for ${tag.runtimeClass.getSimpleName}" in {
//      forAllSimilarA[A]((x, y) => sameResultAsReference(x, y))
//    }
//
//  private def sameResultAsReference[A <: Any: EqualM: Pretty](x: A, y: A): Assertion = {
//    // We are going to override the generated hashCode for our generated AST classes,
//    // so in this test we rely on the underlying implementation from ScalaRuntime,
//    // and hard-code the current definition for the handmade AST classes.
//
//    def reference(self: Any, other: Any): Boolean = (self, other) match {
//      case (left: ParSet, right: ParSet) =>
//        Equiv.by((x: ParSet) => (x.ps, x.remainder, x.connectiveUsed)).equiv(left, right)
//      case (left: ParMap, right: ParMap) =>
//        Equiv.by((x: ParMap) => (x.ps, x.remainder, x.connectiveUsed)).equiv(left, right)
//      case (left: Product, right: Product) =>
//        left.getClass.isInstance(other) &&
//          left.productIterator
//            .zip(right.productIterator)
//            .forall(tupled(reference))
//      case _ => self.equals(other)
//    }
//
//    val referenceResult = reference(x, y)
//    val equalsResult    = x == y
//    val equalMResult    = EqualM[A].equal[Coeval](x, y).value
//
//    withClue(
//      s"""
//         |
//         |Inconsistent results:
//         |
//         |     reference(x, y): $referenceResult
//         |              x == y: $equalsResult
//         |  EqualM.equal(x, y): $equalMResult
//         |
//         |Test data used:
//         |
//         |${Pretty.pretty(x)}
//         |
//         |and
//         |
//         |${Pretty.pretty(y)}
//         |
//         |""".stripMargin
//    ) {
//      // With this check we know that:
//      //   EqualM.equal[Id] == _.equals == reference
//      // This makes this test valid both before and after we override the generated hashCode
//      // (which is long done when you're reading this).
//      referenceResult should be(equalsResult)
//      equalMResult should be(referenceResult)
//    }
//  }
//
//}
