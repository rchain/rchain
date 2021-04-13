package coop.rchain.models

import cats.Show
import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16

object Validator {
  type Validator = ByteString

  val Length = 65

  implicit def ordering: Ordering[Validator] =
    Ordering.by((b: Validator) => b.toByteArray.toIterable)

  implicit val show = new Show[Validator] {
    def show(validator: Validator): String =
      Base16.encode(validator.toByteArray).take(10)
  }
}
