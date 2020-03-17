package coop.rchain.comm.transport

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicBoolean
import coop.rchain.comm.protocol.routing.Protocol
import monix.execution.Callback
import coop.rchain.comm.PeerNode

trait ServerMessage
final case class Send(msg: Protocol) extends ServerMessage
final case class StreamMessage(
    sender: PeerNode,
    typeId: String,
    path: Path,
    compressed: Boolean,
    contentLength: Int,
    extra: String
) extends ServerMessage
