package coop.rchain.models

import coop.rchain.models.BundleOps._
import coop.rchain.models.Expr.ExprInstance.GBool
import org.scalatest.funsuite.AnyFunSuite
import coop.rchain.models.rholang.implicits._

class BundleOpsSpec extends AnyFunSuite {

  test("should merge bundles' properties correctly") {
    val par = Par(exprs = Seq(Expr(GBool(true))))

    withClue("""
            bundle {
              bundle+ { true }
            }
      """) {
      val b2 = Bundle(body = par, writeFlag = true, readFlag = false)
      val b1 = Bundle(b2, writeFlag = true, readFlag = true)

      val result = b1.merge(b2)
      assert(!result.readFlag, "Name should not be readable")
      assert(result.writeFlag, "Name should be writeable")
      assert(
        result.body === par,
        "Flattened bundles should contain process found in the innermost bundle."
      )
    }

    withClue("""
        |bundle+ {
        | bundle- { _body_ }
        |}
      """.stripMargin) {
      val b2 = Bundle(body = par, writeFlag = false, readFlag = true)
      val b1 = Bundle(b2, writeFlag = true, readFlag = false)

      val result = b1.merge(b2)
      assert(!result.readFlag, "Name should not be readable")
      assert(!result.writeFlag, "Name should not be writeable")
      assert(
        result.body === par,
        "Flattened bundles should contain process found in the innermost bundle."
      )
    }

  }

}
