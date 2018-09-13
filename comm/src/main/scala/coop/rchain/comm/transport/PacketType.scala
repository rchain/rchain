package coop.rchain.comm.transport

sealed trait PacketType {
  val id: String
}

case object BlockMessage extends PacketType {
  val id = "BlockMessage"
}

case object BlockRequest extends PacketType {
  val id = "BlockRequest"
}

case object ApprovedBlock extends PacketType {
  val id = "ApprovedBlock"
}

case object ApprovedBlockRequest extends PacketType {
  val id = "ApprovedBlockRequest"
}

case object BlockApproval extends PacketType {
  val id = "BlockApproval"
}

case object UnapprovedBlock extends PacketType {
  val id = "UnapprovedBlock"
}

case object NoApprovedBlockAvailable extends PacketType {
  val id = "NoApprovedBlockAvailable"
}
