package coop.rchain.models.manual

import com.google.protobuf.ByteString

final case class DeployId(
    sig: ByteString = ByteString.EMPTY
)
