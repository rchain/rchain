package coop.rchain.casper.helper

import cats.effect.Concurrent
import coop.rchain.metrics.Span
import coop.rchain.models.{ListParWithRandom, Par}
import coop.rchain.rholang.interpreter.{ContractCall}
import coop.rchain.rholang.interpreter.SystemProcesses.ProcessContext

object CasperInvalidBlocksContract {
  import cats.syntax.all._

  def set[F[_]: Concurrent: Span](
      ctx: ProcessContext[F]
  )(message: Seq[ListParWithRandom]): F[Unit] = {

    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(ackCh, newInvalidBlocks)
          ) =>
        for {
          _ <- ctx.invalidBlocks.setParams(newInvalidBlocks)
          _ <- produce(Seq(Par()), ackCh)
        } yield ()
    }
  }

}
