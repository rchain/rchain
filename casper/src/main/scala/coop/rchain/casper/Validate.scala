package coop.rchain.casper

import cats.Monad
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.casper.protocol.{BlockMessage, Justification}
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

  def blockNumber[F[_]: Monad: Log](block: BlockMessage, dag: BlockDag): F[Boolean] =
    block
      .pure[F]
      .flatMap(b => {
        val parentNumber =
          ProtoUtil.parents(b).map(dag.blockLookup andThen ProtoUtil.blockNumber).max
        val number = ProtoUtil.blockNumber(b)
        if (parentNumber + 1 != number) {
          Log[F].warn(
            ignore(b, s"block number $number is not one more than parent number $parentNumber.")
          ) *> false.pure[F]
        } else {
          true.pure[F]
        }
      })

  def parents[F[_]: Monad: Log](block: BlockMessage,
                                genesis: BlockMessage,
                                dag: BlockDag): F[Boolean] =
    block
      .pure[F]
      .flatMap(b => {
        val latestMessages = b.justifications.map {
          case Justification(v, hash) => v -> hash
        }.toMap
        val viewDag     = dag.copy(latestMessages = latestMessages)
        val estimate    = Estimator.tips(viewDag, genesis)
        val trueParents = ProtoUtil.chooseNonConflicting(estimate, genesis, dag).map(_.blockHash)
        val bParents    = b.header.map(_.parentsHashList).getOrElse(Seq.empty[BlockMessage])

        if (bParents == trueParents)
          true.pure[F]
        else
          Log[F].warn(
            ignore(b, "block parents did not match estimate based on justification")
          ) *> false.pure[F]
      })
}
