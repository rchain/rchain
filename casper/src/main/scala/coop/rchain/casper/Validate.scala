package coop.rchain.casper

import cats.Monad
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil

import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519

import coop.rchain.p2p.effects.Log

object Validate {
  private def ignore(b: BlockMessage, reason: String): String =
    s"CASPER: Ignoring block ${PrettyPrinter.buildString(b.blockHash)} because $reason"

  def blockSignature[F[_]: Monad: Log](block: BlockMessage): F[Boolean] =
    block
      .pure[F]
      .flatMap(b => {
        if (b.sigAlgorithm == "ed25519") {
          val justificationHash = ProtoUtil.protoSeqHash(b.justifications)
          val sigData           = Blake2b256.hash(justificationHash.toByteArray ++ b.blockHash.toByteArray)
          val isValid           = Ed25519.verify(sigData, b.sig.toByteArray, b.sender.toByteArray)

          if (isValid) {
            true.pure[F]
          } else {
            Log[F].warn(ignore(b, "signature is invalid.")) *> false.pure[F]
          }
        } else {
          Log[F].warn(
            ignore(b, s"signature algorithm ${b.sigAlgorithm} is unsupported.")
          ) *> false.pure[F]
        }
      })
}
