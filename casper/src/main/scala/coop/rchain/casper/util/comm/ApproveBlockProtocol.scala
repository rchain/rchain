package coop.rchain.casper.util.comm

import com.google.protobuf.ByteString

import cats.Monad
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._

import coop.rchain.casper.{PrettyPrinter, Validate}
import coop.rchain.casper.protocol._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.PeerNode
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.comm.transport
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.shared._
import coop.rchain.catscontrib.{Capture, IOUtil}

import monix.execution.atomic.AtomicAny

import scala.util.Try

/**
  * Bootstrap side of the protocol defined in
  * https://rchain.atlassian.net/wiki/spaces/CORE/pages/485556483/Initializing+the+Blockchain+--+Protocol+for+generating+the+Genesis+block
  */
class ApproveBlockProtocol[
    F[_]: Capture: Sync: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler] private (
    val block: BlockMessage,
    val requiredSigs: Int,
    val start: Long,
    val duration: Long,
    private val sigsF: Ref[F, Set[Signature]]) {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  val candidate                         = ApprovedBlockCandidate(Some(block), requiredSigs)
  private val u                         = UnapprovedBlock(Some(candidate), start, duration)
  private val serializedUnapprovedBlock = u.toByteString
  private val candidateHash             = PrettyPrinter.buildString(block.blockHash)
  private val sigData                   = Blake2b256.hash(candidate.toByteArray)

  def addApproval(a: BlockApproval): F[Unit] = {
    val validSig = for {
      c   <- a.candidate if c == this.candidate
      sig <- a.sig if Validate.signature(sigData, sig)
    } yield sig
    val sender = a.sig.fold("<Empty Signature>")(sig => Base16.encode(sig.publicKey.toByteArray))

    validSig.fold(Log[F].warn(s"APPROVAL: ignoring invalid block approval from $sender")) { sig =>
      sigsF.update(_ + sig) *> Log[F].info(s"APPROVAL: received block approval from $sender")
    }
  }

  def currentSigs: F[Set[Signature]] = sigsF.get

  def approvedBlock: F[Option[ApprovedBlock]] =
    sigsF.get.map(
      sigs =>
        if (sigs.size >= requiredSigs) Some(ApprovedBlock(Some(candidate), sigs.toSeq))
        else none[ApprovedBlock])

  //TODO: potential optimization, only send to peers we have not
  //      reveived a valid signature from yet
  def sendUnapprovedBlock: F[Unit] =
    for {
      _ <- Log[F].info(s"APPROVAL: Beginning send of UnapprovedBlock $candidateHash to peers...")
      _ <- CommUtil.sendToPeers[F](transport.UnapprovedBlock, serializedUnapprovedBlock)
      _ <- Log[F].info(s"APPROVAL: Sent UnapprovedBlock $candidateHash to peers.")
    } yield ()

  def sendApprovedBlock: F[Boolean] =
    approvedBlock.flatMap {
      case Some(a) =>
        val serializedApprovedBlock = a.toByteString
        for {
          _ <- Log[F].info(s"APPROVAL: Beginning send of ApprovedBlock $candidateHash to peers...")
          _ <- CommUtil.sendToPeers[F](transport.ApprovedBlock, serializedApprovedBlock)
          _ <- Log[F].info(s"APPROVAL: Sent ApprovedBlock $candidateHash to peers.")
        } yield true

      case None => false.pure[F]
    }

}

object ApproveBlockProtocol {

  def apply[F[_]](implicit instance: ApproveBlockProtocol[F]): ApproveBlockProtocol[F] = instance

  def create[F[_]: Capture: Sync: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      block: BlockMessage,
      requiredSigs: Int,
      duration: Long): F[ApproveBlockProtocol[F]] =
    for {
      now   <- Time[F].currentMillis
      sigsF <- Ref.of[F, Set[Signature]](Set.empty[Signature])
    } yield new ApproveBlockProtocol(block, requiredSigs, now, duration, sigsF)

  def blockApprovalPacketHandler[F[_]: Monad: ApproveBlockProtocol](
      peer: PeerNode): PartialFunction[Packet, F[Option[Packet]]] =
    Function
      .unlift(packetToBlockApproval)
      .andThen {
        case ba: BlockApproval =>
          for {
            _ <- ApproveBlockProtocol[F].addApproval(ba)
          } yield none[Packet]
      }

  def run[F[_]: Monad: Capture: Time: ApproveBlockProtocol](waitTime: Long = 5000L): F[Unit] =
    for {
      t             <- Time[F].currentMillis
      maybeApproved <- ApproveBlockProtocol[F].approvedBlock
      _ <- maybeApproved match {
            case Some(_)
                if (ApproveBlockProtocol[F].candidate.requiredSigs == 0 || t >= ApproveBlockProtocol[
                  F].start + ApproveBlockProtocol[F].duration) =>
              ApproveBlockProtocol[F].sendApprovedBlock

            case _ =>
              ApproveBlockProtocol[F].sendUnapprovedBlock *> IOUtil.sleep(waitTime) *> run[F](
                waitTime)
          }
    } yield ()

  private def packetToBlockApproval(msg: Packet): Option[BlockApproval] =
    if (msg.typeId == transport.BlockApproval.id)
      Try(BlockApproval.parseFrom(msg.content.toByteArray)).toOption
    else None

}
