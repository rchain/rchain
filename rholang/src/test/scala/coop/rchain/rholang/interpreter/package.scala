package coop.rchain.rholang.interpreter

import cats._
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.rholang.interpreter.accounting._

package object test {

  def interpreter[M[_]: Sync](): M[Interpreter[M]] =
    for {
      costAlg <- CostAccounting.of[M](Cost.Max)
      cost    = loggingCost[M](costAlg, noOpCostLog)
      result = {
        //implicit val c = cost
        Interpreter[M]
      }
    } yield result

}
