package coop.rchain.models

import com.google.protobuf.ByteString

object Validator {
  type Validator = ByteString

  val Length = 65
}
