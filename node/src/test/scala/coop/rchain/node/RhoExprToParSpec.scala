package coop.rchain.node

import cats.syntax.all._
import coop.rchain.models.syntax._
import coop.rchain.node.api.WebApi._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.chaining._

class RhoExprToParSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  implicit val aArb: Arbitrary[RhoExpr] = Arbitrary(genRhoExpr)

  property("WebAPI RhoExpr => Par => RhoExpr conversion work correct") {
    forAll { rhoExpr: RhoExpr =>
      rhoExprToRhoExpr(rhoExpr) shouldBe rhoExpr.some
    }
  }

  // Converts RhoExpr to Par and then back to RhoExpr
  val rhoExprToRhoExpr = rhoExprToParProto _ andThen exprFromParProto

  val genHexString = Gen.listOf(arbByte.arbitrary).map(_.toArray.toHexString)

  val numOfElements = 2 // StackOverflowError with n >= 3

  // def genExprPar   = listOf2(genRhoExpr).map(ExprPar)
  val genExprTuple  = listOf2(genRhoExpr).map(ExprTuple)
  val genExprList   = listOf2(genRhoExpr).map(ExprList)
  val genExprSet    = listOf2(genRhoExpr).map(_.toSet pipe ExprSet)
  val genExprMap    = Gen.mapOfN(numOfElements, Gen.zip(Gen.alphaNumStr, genRhoExpr)).map(ExprMap)
  val genExprBool   = Gen.resultOf(ExprBool)
  val genExprInt    = Gen.resultOf(ExprInt)
  val genExprString = Gen.resultOf(ExprString)
  val genExprUri    = Gen.resultOf(ExprUri)
  val genExprBytes  = genHexString.map(ExprBytes)
  val genExprUnforg =
    Gen
      .oneOf(
        genHexString.map(UnforgPrivate),
        genHexString.map(UnforgDeploy),
        genHexString.map(UnforgDeployer)
      )
      .map(ExprUnforg)

  lazy val genRhoExpr: Gen[RhoExpr] =
    Gen.lzy(
      Gen.oneOf(
        // genExprPar,
        genExprTuple,
        genExprList,
        genExprSet,
        genExprMap,
        genExprBool,
        genExprInt,
        genExprString,
        genExprUri,
        genExprBytes,
        genExprUnforg
      )
    )

  // Helpers
  def listOf2[A](genA: => Gen[A]) = Gen.listOfN(numOfElements, genA)
}
