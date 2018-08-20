package coop.rchain.casper.util.comm

import cats.Monad
import cats.implicits._
import coop.rchain.casper.ValidatorIdentity
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.Capture
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.transport.CommMessages.packet
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.shared._

import scala.util.Try

/**
  * Validator side of the protocol defined in
  * https://rchain.atlassian.net/wiki/spaces/CORE/pages/485556483/Initializing+the+Blockchain+--+Protocol+for+generating+the+Genesis+block
  */
class BlockApproverProtocol(validatorId: ValidatorIdentity,
                            block: BlockMessage,
                            requiredSigs: Int) {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  private val expectedCandidate  = ApprovedBlockCandidate(Some(block), requiredSigs)
  private val approval           = BlockApproverProtocol.getBlockApproval(expectedCandidate, validatorId)
  private val serializedApproval = approval.toByteString

  def unapprovedBlockPacketHandler[
      F[_]: Capture: Monad: TransportLayer: Log: Time: ErrorHandler: RPConfAsk](
      peer: PeerNode,
      u: UnapprovedBlock): F[Option[Packet]] =
    if (u.candidate.contains(expectedCandidate)) {
      for {
        local <- RPConfAsk[F].reader(_.local)
        msg   = packet(local, transport.BlockApproval, serializedApproval)
        send  <- TransportLayer[F].send(peer, msg)
        _     <- Log[F].info(s"Received expected candidate from $peer. Approval sent in response.")
      } yield none[Packet]
    } else {
      Log[F]
        .warn(s"Received unexpected candidate from $peer")
        .map(_ => none[Packet])
    }
}

object BlockApproverProtocol {
  def getBlockApproval(expectedCandidate: ApprovedBlockCandidate,
                       validatorId: ValidatorIdentity): BlockApproval = {
    val sigData = Blake2b256.hash(expectedCandidate.toByteArray)
    val sig     = validatorId.signature(sigData)
    BlockApproval(Some(expectedCandidate), Some(sig))
  }

  def packetToUnapprovedBlock(msg: Packet): Option[UnapprovedBlock] =
    if (msg.typeId == transport.UnapprovedBlock.id)
      Try(UnapprovedBlock.parseFrom(msg.content.toByteArray)).toOption
    else None
}
