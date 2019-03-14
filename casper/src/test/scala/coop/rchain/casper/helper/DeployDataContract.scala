package coop.rchain.casper.helper
import cats.effect.Sync
import coop.rchain.rholang.interpreter.{ContractCall, RhoType}
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.models.{ListParWithRandomAndPhlos, Par}

object DeployDataContract {
  import cats.implicits._

  def set[F[_]: Sync](
      ctx: SystemProcess.Context[F]
  )(message: (Seq[ListParWithRandomAndPhlos], Int)): F[Unit] = {

    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(RhoType.String("userId"), (pk @ RhoType.ByteArray(_)), ackCh)
          ) =>
        for {
          _ <- ctx.shortLeashParams.updateParams(_.copy(userId = pk))
          _ <- produce(Seq(Par()), ackCh)
        } yield ()
    }
  }
}
