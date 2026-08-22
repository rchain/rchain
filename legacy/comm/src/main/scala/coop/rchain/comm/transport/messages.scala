package coop.rchain.comm.transport

import coop.rchain.comm.PeerNode
import coop.rchain.comm.protocol.routing.Protocol

trait ServerMessage

final case class Send(msg: Protocol) extends ServerMessage

final case class StreamMessage(
    sender: PeerNode,
    typeId: String,
    key: String,
    compressed: Boolean,
    contentLength: Int
) extends ServerMessage
