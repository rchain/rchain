package coop.rchain.casper.genesis

import com.google.protobuf.ByteString

import cats.{Applicative, Monad}
import cats.implicits._

import coop.rchain.casper.{PrettyPrinter, Validate, ValidatorIdentity}
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.comm.CommError.ErrorHandler
import coop.rchain.comm.PeerNode
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.comm.transport
import coop.rchain.comm.transport.TransportLayer
import coop.rchain.shared._

class GenesisValidatorProtocol[
    F[_]: Capture: Monad: NodeDiscovery: TransportLayer: Log: Time: ErrorHandler](
    validatorId: ValidatorIdentity,
    genesis: BlockMessage,
    requiredSigs: Int) {
  private implicit val logSource: LogSource = LogSource(this.getClass)

  private val expectedCandidate = ApprovedBlockCandidate(Some(genesis), requiredSigs)
  private val sigData           = Blake2b256.hash(expectedCandidate.toByteArray)
  private val sig               = validatorId.signature(sigData)

  def unapprovedBlockPacketHandler(peer: PeerNode): PartialFunction[Packet, F[Option[Packet]]] =
    Function
      .unlift(GenesisValidatorProtocol.packetToUnpprovedBlock)
      .andThen {
        case u: UnapprovedBlock =>
          if (u.candidate.contains(expectedCandidate))
            ??? //send BlockApproval message back to peer
          else
            Log[F].warn(s"Received unexpected candidate from $peer").map(_ => none[Packet])
      }
}

object GenesisValidatorProtocol {
  def packetToUnpprovedBlock(msg: Packet): Option[UnapprovedBlock] = ???
}
