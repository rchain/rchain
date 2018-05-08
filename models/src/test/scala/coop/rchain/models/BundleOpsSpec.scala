package coop.rchain.models

import coop.rchain.models.BundleOps._
import coop.rchain.models.Expr.ExprInstance.GBool
import org.scalatest.FunSuite

class BundleOpsSpec extends FunSuite {

  test("should merge bundles' properties correctly") {
    val par = Par(exprs = Seq(Expr(GBool(true))))

    withClue("""
            bundle {
              bundle+ { true }
            }
      """) {
      val b1 = Bundle(None, writeFlag = true, readFlag = true)
      val b2 = Bundle(body = Some(par), writeFlag = true, readFlag = false)

      val result = b1.merge(b2)
      assert(!result.readFlag, "Name should not be readable")
      assert(result.writeFlag, "Name should be writeable")
      assert(result.body.get === par,
             "Flattened bundles should contain process found in the innermost bundle.")
    }

    withClue("""
        |bundle+ {
        | bundle- { _body_ }
        |}
      """.stripMargin) {
      val b1 = Bundle(None, writeFlag = true, readFlag = false)
      val b2 = Bundle(body = Some(par), writeFlag = false, readFlag = true)

      val result = b1.merge(b2)
      assert(!result.readFlag, "Name should not be readable")
      assert(!result.writeFlag, "Name should not be writeable")
      assert(result.body.get === par,
             "Flattened bundles should contain process found in the innermost bundle.")
    }

  }

}
