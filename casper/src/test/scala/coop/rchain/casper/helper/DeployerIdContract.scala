package coop.rchain.casper.helper
import cats.effect.Concurrent
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{GDeployerId, ListParWithRandom, Par}
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.{ContractCall, RhoType}
import coop.rchain.shared.ByteStringOps._

/**
  * Warning: This should under no circumstances be available in production
  */
object DeployerIdContract {
  import cats.implicits._

  def get[F[_]: Concurrent](
      ctx: SystemProcess.Context[F]
  )(message: (Seq[ListParWithRandom], Int)): F[Unit] = {

    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(RhoType.String("deployerId"), RhoType.ByteArray(pk), ackCh)
          ) =>
        for {
          _ <- produce(Seq(GDeployerId(pk.toByteString): Par), ackCh)
        } yield ()
    }
  }
}
