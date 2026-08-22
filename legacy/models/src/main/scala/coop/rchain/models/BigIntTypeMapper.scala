package coop.rchain.models

import com.google.protobuf.ByteString
import scalapb.TypeMapper

object BigIntTypeMapper {
  implicit val bigIntBytesTypeMapper: TypeMapper[ByteString, BigInt] =
    TypeMapper(byteStringToBigInt)(bigIntToByteString)

  private[models] def byteStringToBigInt(byteString: ByteString): BigInt =
    BigInt(byteString.toByteArray)

  private[models] def bigIntToByteString(bigInt: BigInt): ByteString =
    ByteString.copyFrom(bigInt.toByteArray)
}
