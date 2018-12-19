package coop.rchain.rholang.interpreter
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.{Expr, Par}
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount, CostAccounting}

// TODO: After refactoring Reduce to not include implicits in methods,
//       make ChargingReducer extend Reduce.

trait ChargingReducer[F[_]] {

  def phlo: F[CostAccount]

  def setPhlo(phlo: Cost): F[Unit]

  def eval(
      par: Par
  )(implicit env: Env[Par], rand: Blake2b512Random, sequenceNumber: Int = 0): F[Unit]

  def inj(par: Par)(implicit rand: Blake2b512Random): F[Unit]

  def evalExpr(par: Par)(implicit env: Env[Par]): F[Par]

  def evalExprToPar(expr: Expr)(implicit env: Env[Par]): F[Par]

}

object ChargingReducer {

  def apply[F[_]](implicit chargingReducer: ChargingReducer[F]): ChargingReducer[F] =
    chargingReducer

  implicit def chargingReducer[F[_]](
      implicit reducer: Reduce[F],
      costAccounting: CostAccounting[F]
  ): ChargingReducer[F] = new ChargingReducer[F] {
    def phlo: F[CostAccount] =
      costAccounting.get()

    def setPhlo(limit: Cost): F[Unit] =
      costAccounting.set(CostAccount(0, limit))

    def eval(
        par: Par
    )(implicit env: Env[Par], rand: Blake2b512Random, sequenceNumber: Int = 0): F[Unit] =
      reducer.eval(par)

    def inj(par: Par)(implicit rand: Blake2b512Random): F[Unit] =
      reducer.inj(par)

    def evalExpr(par: Par)(implicit env: Env[Par]): F[Par] =
      reducer.evalExpr(par)

    def evalExprToPar(expr: Expr)(implicit env: Env[Par]): F[Par] =
      reducer.evalExprToPar(expr)
  }
}
