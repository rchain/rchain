package coop.rchain.models.manual

import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.AlwaysEqual
import coop.rchain.models.BitSetBytesMapper.bitSetBytesMapper
import scalapb.TypeMapper

import scala.collection.immutable.BitSet

object RhoTypesConst {
  val _typemapper_locallyFree: TypeMapper[ByteString, AlwaysEqual[BitSet]] =
    implicitly[TypeMapper[ByteString, AlwaysEqual[BitSet]]]
  val _typemapper_randomState: TypeMapper[ByteString, Blake2b512Random] =
    implicitly[TypeMapper[ByteString, Blake2b512Random]]
}
