package coop.rchain.casper.helper

import cats.effect.Async
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Span
import coop.rchain.models.ListParWithRandom
import coop.rchain.models.rholang.RhoType
import coop.rchain.rholang.interpreter.{ContractCall, SystemProcesses}

object Secp256k1SignContract {

  def get[F[_]: Async: Span](
      ctx: SystemProcesses.ProcessContext[F]
  )(message: Seq[ListParWithRandom]): F[Unit] = {
    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(RhoType.RhoByteArray(hash), RhoType.RhoByteArray(sk), ackCh)
          ) =>
        val sig = Secp256k1.sign(hash, sk)
        produce(Seq(RhoType.RhoByteArray(sig)), ackCh)
    }
  }
}
