package coop.rchain.models.manual

import com.google.protobuf.ByteString

final case class GDeployerId(
    publicKey: ByteString = ByteString.EMPTY
)
