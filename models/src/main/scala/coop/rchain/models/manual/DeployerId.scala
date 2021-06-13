package coop.rchain.models.manual

import com.google.protobuf.ByteString

final case class DeployerId(
    publicKey: ByteString = ByteString.EMPTY
)
