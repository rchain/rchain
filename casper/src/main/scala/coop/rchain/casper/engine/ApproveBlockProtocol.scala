package coop.rchain.casper.engine

import cats.FlatMap
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.{LastApprovedBlock, PrettyPrinter, Validate, _}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.metrics.Metrics
import coop.rchain.shared
import coop.rchain.shared._

import scala.concurrent.duration._

/**
  * Bootstrap side of the protocol defined in
  * https://rchain.atlassian.net/wiki/spaces/CORE/pages/485556483/Initializing+the+Blockchain+--+Protocol+for+generating+the+Genesis+block
  */
trait ApproveBlockProtocol[F[_]] {
  def addApproval(a: BlockApproval): F[Unit]
  def run(): F[Unit]
}

object ApproveBlockProtocol {

  def apply[F[_]](implicit instance: ApproveBlockProtocol[F]): ApproveBlockProtocol[F] = instance

  //For usage in tests only
  def unsafe[F[_]: Sync: ConnectionsCell: TransportLayer: Log: EventLog: Time: Metrics: RPConfAsk: LastApprovedBlock](
      genesisBlock: BlockMessage,
      requiredSigs: Int,
      duration: FiniteDuration,
      interval: FiniteDuration,
      sigsF: Ref[F, Set[Signature]],
      start: Long
  ): ApproveBlockProtocol[F] =
    new ApproveBlockProtocolImpl[F](
      genesisBlock,
      requiredSigs,
      start,
      duration,
      interval,
      sigsF
    )

  def of[F[_]: Sync: ConnectionsCell: TransportLayer: Log: EventLog: Time: Metrics: RPConfAsk: LastApprovedBlock](
      genesisBlock: BlockMessage,
      requiredSigs: Int,
      duration: FiniteDuration,
      interval: FiniteDuration
  ): F[ApproveBlockProtocol[F]] =
    for {
      now   <- Time[F].currentMillis
      sigsF <- Ref.of[F, Set[Signature]](Set.empty)
    } yield new ApproveBlockProtocolImpl[F](
      genesisBlock,
      requiredSigs,
      now,
      duration,
      interval,
      sigsF
    )

  private class ApproveBlockProtocolImpl[F[_]: Sync: ConnectionsCell: TransportLayer: Log: EventLog: Time: Metrics: RPConfAsk: LastApprovedBlock](
      val genesisBlock: BlockMessage,
      val requiredSigs: Int,
      val start: Long,
      val duration: FiniteDuration,
      val interval: FiniteDuration,
      private val sigsF: Ref[F, Set[Signature]]
  ) extends ApproveBlockProtocol[F] {
    implicit private val logSource: LogSource = LogSource(this.getClass)
    implicit private val metricsSource: Metrics.Source =
      Metrics.Source(CasperMetricsSource, "approve-block")

    private val trustedValidators = genesisBlock.body
      .flatMap(_.state.map(_.bonds.map(_.validator).toSet))
      .getOrElse(Set.empty)
    private val candidate                 = ApprovedBlockCandidate(Some(genesisBlock), requiredSigs)
    private val u                         = UnapprovedBlock(Some(candidate), start, duration.toMillis)
    private val serializedUnapprovedBlock = u.toByteString
    private val candidateHash             = PrettyPrinter.buildString(genesisBlock.blockHash)
    private val sigData                   = Blake2b256.hash(candidate.toByteArray)

    def addApproval(a: BlockApproval): F[Unit] = {
      val validSig = for {
        c   <- a.candidate if c == this.candidate
        sig <- a.sig if Validate.signature(sigData, sig)
      } yield sig

      val trustedValidator =
        if (signedByTrustedValidator(a)) {
          true.pure[F]
        } else {
          Log[F].warn(s"APPROVAL: Received BlockApproval from untrusted validator.") >> false
            .pure[F]
        }

      val isValid = for {
        validValidators <- trustedValidator
      } yield validValidators && validSig.isDefined

      val sender =
        a.sig.fold("<Empty Signature>")(sig => Base16.encode(sig.publicKey.toByteArray))

      FlatMap[F].ifM(isValid)(
        for {
          modifyResult <- sigsF.modify(sigs => {
                           val newSigs = sigs + validSig.get
                           (newSigs, (sigs, newSigs))
                         })
          (before, after) = modifyResult

          _ <- if (after > before)
                Log[F].info("APPROVAL: New signature received") >>
                  Metrics[F].incrementCounter("genesis")
              else
                Log[F].info("APPROVAL: No new sigs received")

          _ <- Log[F].info(s"APPROVAL: received block approval from $sender")

          _ <- Log[F].info(
                s"APPROVAL: ${after.size} approvals received: ${after
                  .map(s => PrettyPrinter.buildString(s.publicKey))
                  .mkString(", ")}"
              )
          _ <- Log[F].info(
                s"APPROVAL: Remaining approvals needed: ${requiredSigs - after.size + 1}"
              )
          _ <- EventLog[F].publish(
                shared.Event.BlockApprovalReceived(
                  a.candidate
                    .flatMap(_.block.map(b => PrettyPrinter.buildStringNoLimit(b.blockHash)))
                    .getOrElse(""),
                  sender
                )
              )
        } yield (),
        Log[F].warn(s"APPROVAL: ignoring invalid block approval from $sender")
      )
    }

    private def signedByTrustedValidator(a: BlockApproval): Boolean =
      a.sig.fold(false)(s => trustedValidators.contains(s.publicKey))

    def run(): F[Unit] =
      Log[F].info("Start execution of ApprovedBlockProtocol") >>
        internalRun() >>
        Log[F].info("Finished execution of ApprovedBlockProtocol")

    private def internalRun(): F[Unit] =
      for {
        _    <- sendUnapprovedBlock
        t    <- Time[F].currentMillis
        sigs <- sigsF.get
        _    <- completeIf(t, sigs)
      } yield ()

    //TODO: potential optimization, only send to peers we have not
    //      received a valid signature from yet
    private def sendUnapprovedBlock: F[Unit] =
      for {
        _ <- Log[F].info(s"APPROVAL: Beginning send of UnapprovedBlock $candidateHash to peers...")
        _ <- CommUtil.streamToPeers[F](transport.UnapprovedBlock, serializedUnapprovedBlock)
        _ <- Log[F].info(s"APPROVAL: Sent UnapprovedBlock $candidateHash to peers.")
        _ <- EventLog[F].publish(shared.Event.SentUnapprovedBlock(candidateHash))
      } yield ()

    private def completeIf(time: Long, signatures: Set[Signature]): F[Unit] =
      if ((time >= start + duration.toMillis && signatures.size >= requiredSigs) || requiredSigs == 0) {
        for {
          _ <- LastApprovedBlock[F].set(ApprovedBlock(Some(candidate), signatures.toSeq))
          _ <- sendApprovedBlock
        } yield ()
      } else Time[F].sleep(interval) >> internalRun()

    private def sendApprovedBlock: F[Unit] =
      for {
        apbO <- LastApprovedBlock[F].get
        _ <- apbO match {
              case None =>
                Log[F].warn(s"APPROVAL: Expected ApprovedBlock but was None.")
              case Some(b) =>
                val serializedApprovedBlock = b.toByteString
                for {
                  _ <- Log[F].info(
                        s"APPROVAL: Beginning send of ApprovedBlock $candidateHash to peers..."
                      )
                  _ <- CommUtil.streamToPeers[F](transport.ApprovedBlock, serializedApprovedBlock)
                  _ <- Log[F].info(s"APPROVAL: Sent ApprovedBlock $candidateHash to peers.")
                  _ <- EventLog[F].publish(shared.Event.SentApprovedBlock(candidateHash))
                } yield ()
            }
      } yield ()
  }
}
