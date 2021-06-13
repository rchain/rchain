package coop.rchain.models.manual

import com.google.protobuf.ByteString
import coop.rchain.models.AlwaysEqual

import scala.collection.immutable.BitSet

/** *
  * A send is written `chan!(data)` or `chan!!(data)` for a persistent send.
  *
  * Upon send, all free variables in data are substituted with their values.
  */
final case class Send(
    chan: Par = Par.defaultInstance,
    data: Seq[Par] = Seq.empty,
    persistent: Boolean = false,
    locallyFree: AlwaysEqual[BitSet] =
      RhoTypesConst._typemapper_locallyFree.toCustom(ByteString.EMPTY),
    connectiveUsed: Boolean = false
)
