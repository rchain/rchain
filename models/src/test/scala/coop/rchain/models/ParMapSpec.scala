package coop.rchain.models

import coop.rchain.models.Channel.ChannelInstance.ChanVar
import coop.rchain.models.Expr.ExprInstance.{EEvalBody, EMapBody, GInt, GString}
import coop.rchain.models.Var.VarInstance.BoundVar
import coop.rchain.models.rholang.implicits._
import org.scalatest.{FlatSpec, Matchers}

class ParMapSpec extends FlatSpec with Matchers {

  "ParMap" should "serialize like EMap" in {
    val map = ParMap(
      Seq[(Par, Par)](
        (GInt(7), GString("Seven")),
        (GInt(7), GString("SeVen")),
        (EVar(BoundVar(1)), EEvalBody(ChanVar(BoundVar(0)))),
        (GInt(2), ParSet(Seq[Par](GInt(2), GInt(1)))),
        (GInt(2), ParSet(Seq[Par](GInt(2))))
      ),
      connectiveUsed = false
    )

    val sortedMap = ParMap(
      Seq[(Par, Par)](
        (GInt(7), GString("Seven")),
        (GInt(7), GString("SeVen")),
        (GInt(2), ParSet(Seq[Par](GInt(2), GInt(1)))),
        (GInt(2), ParSet(Seq[Par](GInt(2)))),
        (EVar(BoundVar(1)), EEvalBody(ChanVar(BoundVar(0))))
      ),
      connectiveUsed = false
    )

    val expr = Expr(EMapBody(sortedMap))

    java.util.Arrays.equals(map.toByteArray, expr.toByteArray) should be(true)
  }

}
