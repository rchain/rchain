package coop.rchain.models.manual

import coop.rchain.models.AlwaysEqual
import scala.collection.immutable.BitSet
import com.google.protobuf.ByteString

/** *
  * `target.method(arguments)`
  */
final case class EMethod(
    methodName: String = "",
    target: Par = Par.defaultInstance,
    arguments: Seq[Par] = Seq.empty,
    locallyFree: AlwaysEqual[BitSet] =
      RhoTypesConst._typemapper_locallyFree.toCustom(ByteString.EMPTY),
    connectiveUsed: Boolean = false
)
