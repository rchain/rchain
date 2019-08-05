package coop.rchain.casper.helper
import cats.effect.Concurrent
import coop.rchain.metrics.Span
import coop.rchain.metrics.Span.TraceId
import coop.rchain.models.ListParWithRandom
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.{ContractCall, RhoType}

/**
  * Warning: This should under no circumstances be available in production
  */
object DeployerIdContract {
  import cats.implicits._

  def get[F[_]: Concurrent: Span](
      ctx: SystemProcess.Context[F]
  )(message: (Seq[ListParWithRandom], Int))(traceId: TraceId): F[Unit] = {

    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(RhoType.String("deployerId"), RhoType.ByteArray(pk), ackCh)
          ) =>
        for {
          _ <- produce(Seq(RhoType.DeployerId(pk)), ackCh)(traceId)
        } yield ()
    }
  }
}
