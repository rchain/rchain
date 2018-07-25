package coop.rchain.casper.util.comm

import com.google.protobuf.ByteString

import cats.{Applicative, Monad}
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

class ApproveBlockProtocol[
    F[_]: Capture: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler] private (
    val candidate: ApprovedBlockCandidate,
    val start: Long,
    val duration: Long) {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  private val u                         = UnapprovedBlock(Some(candidate), start, duration)
  private val serializedUnapprovedBlock = u.toByteString
  private val candidateHash             = candidate.block.fold("")(b => PrettyPrinter.buildString(b.blockHash))
  private val sigData                   = Blake2b256.hash(candidate.toByteArray)

  private val sigsF = new AtomicMonadState[F, Set[Signature]](AtomicAny(Set.empty[Signature]))

  def addApproval(a: BlockApproval): F[Unit] = {
    val validSig = for {
      c   <- a.candidate if c == this.candidate
      sig <- a.sig if Validate.signature(sigData, sig)
    } yield sig
    val sender = a.sig.fold("")(sig => Base16.encode(sig.publicKey.toByteArray))

    validSig.fold(Log[F].warn(s"APPROVAL: ignoring invalid block approval from $sender")) { sig =>
      sigsF.modify(_ + sig) *> Log[F].info(s"APPROVAL: received block approval from $sender")
    }
  }

  def approvedBlock: F[Option[ApprovedBlock]] =
    sigsF.get.map(
      sigs =>
        if (sigs.size >= candidate.requiredSigs) Some(ApprovedBlock(Some(candidate), sigs.toSeq))
        else none[ApprovedBlock])

  //TODO: potential optimization, only send to peers we have not
  //      reveived a valid signature from yet
  def sendUnapprovedBlock: F[Unit] =
    for {
      _ <- Log[F].info(s"APPROVAL: Beginning send of UnapprovedBlock $candidateHash to peers...")
      _ <- CommUtil.sendToPeers[F](transport.UnapprovedBlock, serializedUnapprovedBlock)
      _ <- Log[F].info(s"APPROVAL: Sent UnapprovedBlock $candidateHash to peers.")
    } yield ()

}

object ApproveBlockProtocol {

  def apply[F[_]](implicit instance: ApproveBlockProtocol[F]): ApproveBlockProtocol[F] = instance

  def create[F[_]: Capture: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
      candidate: ApprovedBlockCandidate,
      duration: Long): F[ApproveBlockProtocol[F]] =
    Time[F].currentMillis.map(now => new ApproveBlockProtocol(candidate, now, duration))

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
            case Some(approvedBlock)
                if (t >= ApproveBlockProtocol[F].start + ApproveBlockProtocol[F].duration) =>
              ??? //Done! Send approved block out

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
