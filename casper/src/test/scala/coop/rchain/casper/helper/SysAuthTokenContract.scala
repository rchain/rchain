package coop.rchain.casper.helper

import cats.effect.Concurrent
import coop.rchain.metrics.Span
import coop.rchain.models.{GSysAuthToken, ListParWithRandom}
import coop.rchain.rholang.interpreter.{ContractCall, RhoType}
import coop.rchain.rholang.interpreter.Runtime.SystemProcess

/**
  * Warning: This should under no circumstances be available in production
  */
object SysAuthTokenContract {
  import cats.implicits._

  def get[F[_]: Concurrent: Span](
      ctx: SystemProcess.Context[F]
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
