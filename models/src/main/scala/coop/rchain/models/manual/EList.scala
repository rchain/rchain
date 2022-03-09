package coop.rchain.models.manual

import coop.rchain.models.AlwaysEqual
import scala.collection.immutable.BitSet
import com.google.protobuf.ByteString

final case class EList(
    ps: Seq[Par] = Seq.empty,
    locallyFree: AlwaysEqual[BitSet] =
      RhoTypesConst._typemapper_locallyFree.toCustom(ByteString.EMPTY),
    connectiveUsed: Boolean = false,
    remainder: Option[Var] = None
)
