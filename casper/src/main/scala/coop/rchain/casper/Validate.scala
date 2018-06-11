package coop.rchain.casper

import cats.Applicative
import cats.implicits._

import com.google.protobuf.ByteString

import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import coop.rchain.casper.util.ProtoUtil

import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519

import coop.rchain.p2p.effects.Log

import scala.util.Try

object Validate {
  def ignore(b: BlockMessage, reason: String): String =
    s"CASPER: Ignoring block ${PrettyPrinter.buildString(b.blockHash)} because $reason"

  def blockSignature[F[_]: Applicative: Log](b: BlockMessage): F[Boolean] =
    if (b.sigAlgorithm == "ed25519") {
      val justificationHash = ProtoUtil.protoSeqHash(b.justifications)
      val sigData =
        Blake2b256.hash(justificationHash.toByteArray ++ b.blockHash.toByteArray)
      val isValid =
        Try(Ed25519.verify(sigData, b.sig.toByteArray, b.sender.toByteArray)).getOrElse(false)

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

  def blockNumber[F[_]: Applicative: Log](b: BlockMessage, dag: BlockDag): F[Boolean] = {
    val parentNumber = ProtoUtil
      .parents(b)
      .headOption
      .map(dag.blockLookup andThen ProtoUtil.blockNumber)
    val number = ProtoUtil.blockNumber(b)
    val result = parentNumber.fold(number == 0)(_ + 1 == number)

    if (result) {
      true.pure[F]
    } else {
      val log = parentNumber.fold(
        Log[F].warn(
          ignore(b, s"block number $number is not zero, but block has no parents.")
        )
      )(n => {
        Log[F].warn(
          ignore(b, s"block number $number is not one more than parent number $n.")
        )
      })

      log *> false.pure[F]
    }
  }

  def blockSender[F[_]: Applicative: Log](b: BlockMessage,
                                          genesis: BlockMessage,
                                          dag: BlockDag): F[Boolean] =
    if (b == genesis) {
      true.pure[F] //genesis block has a valid sender
    } else {
      val weight = ProtoUtil.weightFromSender(b, dag.blockLookup)
      if (weight > 0) true.pure[F]
      else
        Log[F].warn(
          ignore(b, s"because block creator ${PrettyPrinter.buildString(b.sender)} has 0 weight.")
        ) *> false.pure[F]
    }

  def parents[F[_]: Applicative: Log](b: BlockMessage,
                                      genesis: BlockMessage,
                                      dag: BlockDag): F[Boolean] = {
    val bParents = b.header.fold(Seq.empty[ByteString])(_.parentsHashList)

    if (b.justifications.isEmpty) {
      if (bParents.exists(_ != genesis.blockHash))
        Log[F].warn(
          ignore(b, "justification is empty, but block has non-genesis parents.")
        ) *> false.pure[F]
      else
        true.pure[F]
    } else {
      val latestMessages = b.justifications
        .foldLeft(Map.empty[Validator, BlockHash]) {
          case (map, Justification(v, hash)) => map.updated(v, hash)
        }
      val viewDag     = dag.copy(latestMessages = latestMessages)
      val estimate    = Estimator.tips(viewDag, genesis)
      val trueParents = ProtoUtil.chooseNonConflicting(estimate, genesis, dag).map(_.blockHash)

      if (bParents == trueParents)
        true.pure[F]
      else
        Log[F].warn(
          ignore(b, "block parents did not match estimate based on justification.")
        ) *> false.pure[F]
    }
  }
}
