package coop.rchain.casper.helper

import cats.effect.Concurrent
import coop.rchain.metrics.Span
import coop.rchain.models.rholang.RhoType
import coop.rchain.models.{GSysAuthToken, ListParWithRandom}
import coop.rchain.rholang.interpreter.ContractCall
import coop.rchain.rholang.interpreter.SystemProcesses.ProcessContext

/**
  * Warning: This should under no circumstances be available in production
  */
object SysAuthTokenContract {
  import cats.syntax.all._

  def get[F[_]: Concurrent: Span](
      ctx: ProcessContext[F]
  )(message: Seq[ListParWithRandom]): F[Unit] = {

    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(ackCh)
          ) =>
        for {
          _ <- produce(Seq(RhoType.SysAuthToken(GSysAuthToken())), ackCh)
        } yield ()
    }
  }
}
