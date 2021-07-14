package coop.rchain.models.manual

import com.google.protobuf.ByteString

final case class GDeployId(
    sig: ByteString = ByteString.EMPTY
)
