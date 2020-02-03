package coop.rchain.casper.helper

import cats.effect.Concurrent
import coop.rchain.crypto.PublicKey
import coop.rchain.metrics.Span
import coop.rchain.models.{ListParWithRandom, Par}
import coop.rchain.rholang.interpreter.{ContractCall, RhoType}
import coop.rchain.rholang.interpreter.Runtime.SystemProcess

object CasperInvalidBlocksContract {
  import cats.implicits._

  def set[F[_]: Concurrent: Span](
      ctx: SystemProcess.Context[F]
  )(message: Seq[ListParWithRandom]): F[Unit] = {

    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(newInvalidBlocks, ackCh)
          ) =>
        for {
          _ <- ctx.invalidBlocks.setParams(newInvalidBlocks)
          _ <- produce(Seq(Par()), ackCh)
        } yield ()
    }
  }

}
