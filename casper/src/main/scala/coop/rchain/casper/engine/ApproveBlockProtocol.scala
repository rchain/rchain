package coop.rchain.casper.engine

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.LastApprovedBlock.LastApprovedBlock
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.Genesis.createGenesisBlock
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.comm.CommUtil
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.util.{BondsParser, VaultParser}
import coop.rchain.casper.{LastApprovedBlock, PrettyPrinter, Validate, _}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.metrics.Metrics
import coop.rchain.shared
import coop.rchain.shared._

import java.nio.file.Path
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
  def unsafe[F[_]: Sync: CommUtil: Log: EventLog: Time: Metrics: LastApprovedBlock](
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

  def of[F[_]: Concurrent: ContextShift: CommUtil: Log: EventLog: Time: Metrics: RuntimeManager: LastApprovedBlock](
      bondsPath: String,
      autogenShardSize: Int,
      genesisPath: Path,
      vaultsPath: String,
      minimumBond: Long,
      maximumBond: Long,
      epochLength: Int,
      quarantineLength: Int,
      numberOfActiveValidators: Int,
      shardId: String,
      deployTimestamp: Option[Long],
      requiredSigs: Int,
      duration: FiniteDuration,
      interval: FiniteDuration,
      blockNumber: Long,
      sender: ByteString
  ): F[ApproveBlockProtocol[F]] =
    for {
      now       <- Time[F].currentMillis
      timestamp = deployTimestamp.getOrElse(now)

      vaults <- VaultParser.parse[F](vaultsPath)
      bonds <- BondsParser.parse[F](
                bondsPath,
                autogenShardSize
              )

      genesisBlock <- if (bonds.size <= requiredSigs)
                       Sync[F].raiseError[BlockMessage](
                         new Exception(
                           "Required sigs must be smaller than the number of bonded validators"
                         )
                       )
                     else {
                       val validators = bonds.toSeq.map(Validator.tupled)
                       createGenesisBlock(
                         implicitly[RuntimeManager[F]],
                         Genesis(
                           sender = sender,
                           shardId = shardId,
                           timestamp = timestamp,
                           proofOfStake = ProofOfStake(
                             minimumBond = minimumBond,
                             maximumBond = maximumBond,
                             epochLength = epochLength,
                             quarantineLength = quarantineLength,
                             numberOfActiveValidators = numberOfActiveValidators,
                             validators = validators
                           ),
                           vaults = vaults,
                           supply = Long.MaxValue,
                           blockNumber = blockNumber
                         )
                       )
                     }
      sigsF <- Ref.of[F, Set[Signature]](Set.empty)

    } yield new ApproveBlockProtocolImpl[F](
      genesisBlock,
      requiredSigs,
      now,
      duration,
      interval,
      sigsF
    )

  private class ApproveBlockProtocolImpl[F[_]: Sync: CommUtil: Log: EventLog: Time: Metrics: LastApprovedBlock](
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

    private val trustedValidators         = genesisBlock.body.state.bonds.map(_.validator).toSet
    private val candidate                 = ApprovedBlockCandidate(genesisBlock, requiredSigs)
    private val u                         = UnapprovedBlock(candidate, start, duration.toMillis)
    private val serializedUnapprovedBlock = ToPacket(u.toProto)
    private val candidateHash             = PrettyPrinter.buildString(genesisBlock.blockHash)
    private val sigData                   = Blake2b256.hash(candidate.toProto.toByteArray)

    def addApproval(a: BlockApproval): F[Unit] = {
      val validSig =
        (a.candidate == this.candidate) && Validate.signature(sigData, a.sig)

      val sender = Base16.encode(a.sig.publicKey.toByteArray)

      if (signedByTrustedValidator(a)) {
        if (validSig) {
          for {
            modifyResult <- sigsF.modify(sigs => {
                             val newSigs = sigs + a.sig
                             (newSigs, (sigs, newSigs))
                           })
            (before, after) = modifyResult

            _ <- if (after > before)
                  Log[F].info("New signature received") >>
                    Metrics[F].incrementCounter("genesis")
                else
                  Log[F].info("No new sigs received")

            _ <- Log[F].info(s"Received block approval from $sender")

            _ <- Log[F].info(
                  s"${after.size} approvals received: ${after
                    .map(s => PrettyPrinter.buildString(s.publicKey))
                    .mkString(", ")}"
                )

            _ <- EventLog[F].publish(
                  shared.Event.BlockApprovalReceived(
                    PrettyPrinter.buildStringNoLimit(a.candidate.block.blockHash),
                    sender
                  )
                )

          } yield ()
        } else Log[F].warn(s"Ignoring invalid block approval from $sender")
      } else Log[F].warn(s"Received BlockApproval from untrusted validator.")
    }

    private def signedByTrustedValidator(a: BlockApproval): Boolean =
      trustedValidators.contains(a.sig.publicKey)

    def run(): F[Unit] =
      Log[F].info(
        s"Starting execution of ApprovedBlockProtocol. " +
          s"Waiting for $requiredSigs approvals from genesis validators."
      ) >> {
        if (requiredSigs > 0)
          internalRun()
        else
          Log[F].info("Self-approving genesis block.") >>
            completeGenesisCeremoy(Set.empty[Signature]) >>
            Log[F].info("Finished execution of ApprovedBlockProtocol")
      }

    private def internalRun(): F[Unit] =
      for {
        _    <- sendUnapprovedBlock
        _    <- Time[F].sleep(interval)
        t    <- Time[F].currentMillis
        sigs <- sigsF.get
        _    <- completeIf(t, sigs)
      } yield ()

    //TODO: potential optimization, only send to peers we have not
    //      received a valid signature from yet
    private def sendUnapprovedBlock: F[Unit] =
      for {
        _ <- Log[F].info(s"Broadcasting UnapprovedBlock $candidateHash...")
        _ <- CommUtil[F].streamToPeers(serializedUnapprovedBlock)
        _ <- EventLog[F].publish(shared.Event.SentUnapprovedBlock(candidateHash))
      } yield ()

    private def completeIf(time: Long, signatures: Set[Signature]): F[Unit] =
      if ((time >= start + duration.toMillis && signatures.size >= requiredSigs) || requiredSigs == 0) {
        completeGenesisCeremoy(signatures)
      } else
        Log[F].info(
          s"Failed to meet approval conditions. " +
            s"Signatures: ${signatures.size} of ${requiredSigs} required. " +
            s"Duration ${time - start} ms of ${duration.toMillis} ms minimum. " +
            s"Continue broadcasting UnapprovedBlock..."
        ) >> internalRun()

    private def completeGenesisCeremoy(signatures: Set[Signature]): F[Unit] =
      for {
        _ <- LastApprovedBlock[F].set(ApprovedBlock(candidate, signatures.toList))
        _ <- sendApprovedBlock
      } yield ()

    private def sendApprovedBlock: F[Unit] =
      for {
        apbO <- LastApprovedBlock[F].get
        _ <- apbO match {
              case None =>
                Log[F].warn(s"Expected ApprovedBlock but was None.")
              case Some(b) =>
                val serializedApprovedBlock = ToPacket(b.toProto)
                for {
                  _ <- Log[F].info(
                        s"Sending ApprovedBlock $candidateHash to peers..."
                      )
                  _ <- CommUtil[F].streamToPeers(serializedApprovedBlock)
                  _ <- EventLog[F].publish(shared.Event.SentApprovedBlock(candidateHash))
                } yield ()
            }
      } yield ()
  }
}
