package coop.rchain.models.manual

import com.google.protobuf.ByteString
import coop.rchain.models.AlwaysEqual

import scala.collection.immutable.BitSet

/** Number of variables bound in the new statement.
  * For normalized form, p should not contain solely another new.
  * Also for normalized form, the first use should be level+0, next use level+1
  * up to level+count for the last used variable.
  *
  * @param bindCount
  *   Includes any uris listed below. This makes it easier to substitute or walk a term.
  * @param uri
  *   For normalization, uri-referenced variables come at the end, and in lexicographical order.
  */
final case class New(
    bindCount: Int = 0,
    p: Par = Par.defaultInstance,
    uri: Seq[String] = Seq.empty,
    injections: Map[String, Par] = Map.empty,
    locallyFree: AlwaysEqual[BitSet] =
      RhoTypesConst._typemapper_locallyFree.toCustom(ByteString.EMPTY)
)
