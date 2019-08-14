package coop.rchain.casper.helper

import cats.effect.Concurrent
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Span
import coop.rchain.models.ListParWithRandom
import coop.rchain.rholang.interpreter.Runtime.SystemProcess
import coop.rchain.rholang.interpreter.{ContractCall, RhoType}

object Secp256k1SignContract {

  def get[F[_]: Concurrent: Span](
      ctx: SystemProcess.Context[F]
  )(message: (Seq[ListParWithRandom], Int)): F[Unit] = {
    val isContractCall = new ContractCall(ctx.space, ctx.dispatcher)
    message match {
      case isContractCall(
          produce,
          Seq(RhoType.ByteArray(hash), RhoType.ByteArray(sk), ackCh)
          ) =>
        val sig = Secp256k1.sign(hash, sk)
        produce(Seq(RhoType.ByteArray(sig)), ackCh)
    }
  }
}
