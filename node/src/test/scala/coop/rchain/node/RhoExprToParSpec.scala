package coop.rchain.node

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.models.syntax._
import coop.rchain.models.testImplicits._
import coop.rchain.node.api.WebApi._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class RhoExprToParSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  private def roundTripTest(expr: RhoExpr): Option[RhoExpr] =
    exprFromParProto(rhoExprToParProto(expr))

  // Simple types

  property("WebAPI ExprBool <-> Par conversion work correct") {
    forAll { b: Boolean =>
      roundTripTest(ExprBool(b)).collect { case ExprBool(data) => data } shouldBe b.some
    }
  }

  property("WebAPI ExprInt <-> Par conversion work correct") {
    forAll { i: Long =>
      roundTripTest(ExprInt(i)).collect { case ExprInt(data) => data } shouldBe i.some
    }
  }

  property("WebAPI ExprString <-> Par conversion work correct") {
    forAll { s: String =>
      roundTripTest(ExprString(s)).collect { case ExprString(data) => data } shouldBe s.some
    }
  }

  property("WebAPI ExprUri <-> Par conversion work correct") {
    forAll { uri: String =>
      roundTripTest(ExprUri(uri)).collect { case ExprUri(data) => data } shouldBe uri.some
    }
  }

  property("WebAPI ExprUnforg <-> Par conversion work correct") {
    forAll { bs: ByteString =>
      roundTripTest(ExprBytes(bs.toHexString)).collect { case ExprBytes(data) => data } shouldBe bs.toHexString.some
    }
  }

  property("WebAPI ExprBytes <-> Par conversion work correct") {
    forAll { bs: ByteString =>
      roundTripTest(ExprUnforg(UnforgDeploy(bs.toHexString))).collect {
        case ExprUnforg(data) => data
      } shouldBe UnforgDeploy(bs.toHexString).some
    }
  }

  // Nested types

  implicit val listRhoExprArb: Arbitrary[List[RhoExpr]] = Arbitrary(
    for {
      bool <- arbBool.arbitrary
      long <- arbLong.arbitrary
      str  <- arbString.arbitrary
      uri  <- arbString.arbitrary
      bs   <- arbByteArray.arbitrary
    } yield List[RhoExpr](
      ExprBool(bool),
      ExprInt(long),
      ExprString(str),
      ExprUri(uri),
      ExprBytes(bs.toHexString)
    )
  )

  property("WebAPI ExprPar <-> Par conversion work correct") {
    forAll { listExpr: List[RhoExpr] =>
      roundTripTest(ExprPar(listExpr)).collect { case ExprPar(data) => data } shouldBe listExpr.some
    }
  }

  property("WebAPI ExprTuple <-> Par conversion work correct") {
    forAll { listExpr: List[RhoExpr] =>
      roundTripTest(ExprTuple(listExpr)).collect { case ExprTuple(data) => data } shouldBe listExpr.some
    }
  }

  property("WebAPI ExprList <-> Par conversion work correct") {
    forAll { listExpr: List[RhoExpr] =>
      roundTripTest(ExprList(listExpr)).collect { case ExprList(data) => data } shouldBe listExpr.some
    }
  }

  property("WebAPI ExprSet <-> Par conversion work correct") {
    forAll { listExpr: List[RhoExpr] =>
      roundTripTest(ExprSet(listExpr)).collect { case ExprSet(data) => data } shouldBe listExpr.some
    }
  }

  implicit val mapRhoExprArb: Arbitrary[Map[String, RhoExpr]] = Arbitrary(
    for {
      bool <- arbBool.arbitrary
      long <- arbLong.arbitrary
      str  <- arbString.arbitrary
      uri  <- arbString.arbitrary
      bs   <- arbByteArray.arbitrary
      list <- listRhoExprArb.arbitrary
      keys = (1 to 6).map(_ => arbString.arbitrary.sample.get)
    } yield Map[String, RhoExpr](
      keys(0) -> ExprBool(bool),
      keys(1) -> ExprInt(long),
      keys(2) -> ExprString(str),
      keys(3) -> ExprUri(uri),
      keys(4) -> ExprBytes(bs.toHexString),
      keys(5) -> ExprList(list)
    )
  )

  property("WebAPI ExprMap <-> Par conversion work correct") {
    forAll { mapExpr: Map[String, RhoExpr] =>
      roundTripTest(ExprMap(mapExpr)).collect { case ExprMap(data) => data } shouldBe mapExpr.some
    }
  }
}
