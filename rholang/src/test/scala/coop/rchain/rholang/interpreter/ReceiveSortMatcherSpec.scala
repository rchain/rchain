package coop.rchain.rholang.interpreter

import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.{Par, ReceiveBind, Var}
import coop.rchain.models.Var.VarInstance.FreeVar
import monix.eval.Coeval
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.{DeBruijnLevelMap, ReceiveBindsSortMatcher, VarSort}

class ReceiveSortMatcherSpec extends FlatSpec with Matchers {
  val emptyMap = DeBruijnLevelMap.empty[VarSort]
  "Binds" should "Presort based on their channel and then pattern" in {
    val binds: List[Tuple4[List[Par], Par, Option[Var], DeBruijnLevelMap[VarSort]]] =
      List(
        (
          List(GInt(2)),
          GInt(3),
          None,
          emptyMap
        ),
        (
          List(GInt(3)),
          GInt(2),
          None,
          emptyMap
        ),
        (
          List(GInt(3)),
          GInt(2),
          Some(FreeVar(0)),
          emptyMap
        ),
        (
          List(GInt(1)),
          GInt(3),
          None,
          emptyMap
        )
      )
    val sortedBinds: List[Tuple2[ReceiveBind, DeBruijnLevelMap[VarSort]]] =
      List(
        (
          ReceiveBind(
            List(GInt(3)),
            GInt(2),
            None
          ),
          emptyMap
        ),
        (
          ReceiveBind(
            List(GInt(3)),
            GInt(2),
            Some(FreeVar(0))
          ),
          emptyMap
        ),
        (
          ReceiveBind(
            List(GInt(1)),
            GInt(3),
            None
          ),
          emptyMap
        ),
        (
          ReceiveBind(
            List(GInt(2)),
            GInt(3),
            None
          ),
          emptyMap
        )
      )
    val result = ReceiveBindsSortMatcher.preSortBinds[Coeval, VarSort](binds).value
    result should be(sortedBinds)
  }
}
