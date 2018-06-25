package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.{Channel, Par, ReceiveBind, Var}
import coop.rchain.models.Var.VarInstance.FreeVar
import monix.eval.Coeval
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.rholang.implicits._

class ReceiveSortMatcherSpec extends FlatSpec with Matchers {
  val emptyMap = DebruijnLevelMap[VarSort]()
  val p        = Par()
  "Binds" should "Presort based on their channel and then pattern" in {
    val binds: List[Tuple4[List[Channel], Channel, Option[Var], DebruijnLevelMap[VarSort]]] =
      List(
        (
          List(Quote(GInt(2))),
          Quote(GInt(3)),
          None,
          emptyMap
        ),
        (
          List(Quote(GInt(3))),
          Quote(GInt(2)),
          None,
          emptyMap
        ),
        (
          List(Quote(GInt(3))),
          Quote(GInt(2)),
          Some(FreeVar(0)),
          emptyMap
        ),
        (
          List(Quote(GInt(1))),
          Quote(GInt(3)),
          None,
          emptyMap
        )
      )
    val sortedBinds: List[Tuple2[ReceiveBind, DebruijnLevelMap[VarSort]]] =
      List(
        (
          ReceiveBind(
            List(Quote(GInt(3))),
            Quote(GInt(2)),
            None,
          ),
          emptyMap
        ),
        (
          ReceiveBind(
            List(Quote(GInt(3))),
            Quote(GInt(2)),
            Some(FreeVar(0)),
          ),
          emptyMap
        ),
        (
          ReceiveBind(
            List(Quote(GInt(1))),
            Quote(GInt(3)),
            None,
          ),
          emptyMap
        ),
        (
          ReceiveBind(
            List(Quote(GInt(2))),
            Quote(GInt(3)),
            None,
          ),
          emptyMap
        )
      )
    val result = ReceiveBindsSortMatcher.preSortBinds[Coeval, VarSort](binds)
    result should be(sortedBinds)
  }
}
