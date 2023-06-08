package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.rholang.interpreter.accounting.CostAccounting.CostStateRef
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError

package object accounting extends Costs {

  def charge[F[_]: Sync: CostStateRef](amount: Cost): F[Unit] =
    for {
      isError <- CostStateRef[F].modify(_.charge(amount))
      _       <- OutOfPhlogistonsError.raiseError.whenA(isError)
    } yield ()
}
