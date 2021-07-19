package com.revdefine.origin.revvaultexport

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{Expr, GPrivate, Par, Send}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.accounting.Cost

import scala.util.Random

object VaultBalanceGetter {

  private def newReturnName: Par =
    GPrivate(ByteString.copyFromUtf8(Random.alphanumeric.take(10).foldLeft("")(_ + _)))
  private def getBalancePar(vaultPar: Par, returnChannel: Par) =
    Par(
      sends = Seq(
        Send(
          chan = vaultPar,
          data = Seq(
            Par(exprs = Seq(Expr(GString("balance")))),
            returnChannel
          )
        )
      )
    )

  def getBalanceFromVaultPar[F[_]: Sync](vaultPar: Par, runtime: RhoRuntime[F]): F[Option[Long]] =
    for {
      _             <- runtime.cost.set(Cost.UNSAFE_MAX)
      ret           = VaultBalanceGetter.newReturnName
      getBalancePar = VaultBalanceGetter.getBalancePar(vaultPar, ret)
      _             <- runtime.inj(getBalancePar)(Blake2b512Random(10))
      data          <- runtime.getData(ret)
      result = data.headOption.flatMap(
        d =>
          d.a.pars match {
            case headPar +: Nil =>
              headPar.exprs match {
                case headExpr +: Nil =>
                  headExpr.exprInstance match {
                    case GInt(i) => Some(i)
                    case _       => None
                  }
                case _ => None
              }
            case _ => None
          }
      )
    } yield result

  def getAllVaultBalance[F[_]: Sync](
      vaultTreeHashMapDepth: Int,
      vaultChannel: Par,
      runtime: RhoRuntime[F]
  ): F[List[(ByteString, Long)]] =
    for {
      vaultMap <- RhoTrieTraverser.traverseTrie(vaultTreeHashMapDepth, vaultChannel, runtime)
      extracted = RhoTrieTraverser.vecParMapToMap(
        vaultMap,
        p => p.exprs.head.getGByteArray,
        p => p
      )
      result <- extracted.toList.traverse {
                 case (key, vaultPar) =>
                   for {
                     balance <- getBalanceFromVaultPar(vaultPar, runtime)
                   } yield (key, balance.get)
               }
    } yield result
}
