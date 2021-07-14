package coop.rchain.models.manual

import com.google.protobuf.ByteString

final case class GPrivate(
    id: ByteString = ByteString.EMPTY
)
