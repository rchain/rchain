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
