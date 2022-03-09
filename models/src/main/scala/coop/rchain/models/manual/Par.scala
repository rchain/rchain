package coop.rchain.models.manual

import com.google.protobuf.ByteString
import coop.rchain.models.AlwaysEqual

import scala.collection.immutable.BitSet

/** *
  * Rholang process
  *
  * For example, `&#64;0!(1) | &#64;2!(3) | for(x &lt;- &#64;0) { Nil }` has two sends
  * and one receive.
  *
  * The Nil process is a `Par` with no sends, receives, etc.
  *
  * @param unforgeables
  *   unforgeable names
  */
final case class Par(
    sends: Seq[Send] = Seq.empty,
    receives: Seq[Receive] = Seq.empty,
    news: Seq[New] = Seq.empty,
    exprs: Seq[Expr] = Seq.empty,
    matches: Seq[Match] = Seq.empty,
    unforgeables: Seq[GUnforgeable] = Seq.empty,
    bundles: Seq[Bundle] = Seq.empty,
    connectives: Seq[Connective] = Seq.empty,
    locallyFree: AlwaysEqual[BitSet] = RhoTypesConst._typemapper_locallyFree
      .toCustom(ByteString.EMPTY),
    connectiveUsed: Boolean = false
)

object Par {
  lazy val defaultInstance: Par = Par(
    sends = Seq.empty,
    receives = Seq.empty,
    news = Seq.empty,
    exprs = Seq.empty,
    matches = Seq.empty,
    unforgeables = Seq.empty,
    bundles = Seq.empty,
    connectives = Seq.empty,
    locallyFree = RhoTypesConst._typemapper_locallyFree.toCustom(ByteString.EMPTY)
  )
}
