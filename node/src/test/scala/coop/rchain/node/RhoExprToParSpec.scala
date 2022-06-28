package coop.rchain.node

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.models.syntax._
import coop.rchain.node.api.WebApi._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class RhoExprToParSpec extends AnyWordSpec with Matchers {
  private def roundTripTest(expr: RhoExpr): Option[RhoExpr] =
    exprFromParProto(rhoExprToParProto(expr))

  "WebAPI RhoExpr <-> Par conversion" should {
    "work correct with simple types" in {
      roundTripTest(ExprBool(true)).collect { case ExprBool(data)            => data } shouldBe true.some
      roundTripTest(ExprInt(42)).collect { case ExprInt(data)                => data } shouldBe 42.some
      roundTripTest(ExprString("abc")).collect { case ExprString(data)       => data } shouldBe "abc".some
      roundTripTest(ExprUri("http://test.com")).collect { case ExprUri(data) => data } shouldBe "http://test.com".some

      val bytesAsString = ByteString.copyFrom(Array[Byte](1, 2, 3)).toHexString
      roundTripTest(ExprBytes(bytesAsString)).collect { case ExprBytes(data) => data } shouldBe bytesAsString.some
      roundTripTest(ExprUnforg(UnforgDeploy(bytesAsString))).collect {
        case ExprUnforg(data) => data
      } shouldBe UnforgDeploy(
        bytesAsString
      ).some
    }
    "work correct with nested types" in {
      val listExpr = List[RhoExpr](ExprBool(true), ExprInt(42))
      roundTripTest(ExprPar(listExpr)).collect { case ExprPar(data)     => data } shouldBe listExpr.some
      roundTripTest(ExprTuple(listExpr)).collect { case ExprTuple(data) => data } shouldBe listExpr.some
      roundTripTest(ExprList(listExpr)).collect { case ExprList(data)   => data } shouldBe listExpr.some
      roundTripTest(ExprSet(listExpr)).collect { case ExprSet(data)     => data } shouldBe listExpr.some

      val mapExpr =
        Map[String, RhoExpr]("a" -> ExprBool(true), "b" -> ExprInt(42), "c" -> ExprList(listExpr))
      roundTripTest(ExprMap(mapExpr)).collect { case ExprMap(data) => data } shouldBe mapExpr.some
    }
  }
}
