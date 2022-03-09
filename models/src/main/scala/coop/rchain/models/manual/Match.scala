package coop.rchain.models.manual

import com.google.protobuf.ByteString
import coop.rchain.models.AlwaysEqual
import scala.collection.immutable.BitSet

final case class Match(
    target: Par = Par.defaultInstance,
    cases: Seq[MatchCase] = Seq.empty,
    locallyFree: AlwaysEqual[BitSet] =
      RhoTypesConst._typemapper_locallyFree.toCustom(ByteString.EMPTY),
    connectiveUsed: Boolean = false
)
