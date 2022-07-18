package coop.rchain.casper.helper

import cats.effect.Concurrent
import coop.rchain.crypto.PublicKey
import coop.rchain.metrics.Span
import coop.rchain.models.rholang.RhoType
import coop.rchain.models.{ListParWithRandom, Par}
import coop.rchain.rholang.interpreter.ContractCall
import coop.rchain.rholang.interpreter.SystemProcesses.ProcessContext

object BlockDataContract {
  import cats.syntax.all._

  def set[F[_]: Concurrent: Span](
      ctx: ProcessContext[F]
  )(message: Seq[ListParWithRandom]): F[Unit] = {

    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(RhoType.RhoString("sender"), RhoType.RhoByteArray(pk), ackCh)
          ) =>
        for {
          _ <- ctx.blockData.update(_.copy(sender = PublicKey(pk)))
          _ <- produce(Seq(Par()), ackCh)
        } yield ()

      case isContractCall(
          produce,
          Seq(RhoType.RhoString("blockNumber"), RhoType.RhoNumber(n), ackCh)
          ) =>
        for {
          _ <- ctx.blockData.update(_.copy(blockNumber = n))
          _ <- produce(Seq(Par()), ackCh)
        } yield ()
    }
  }
}
