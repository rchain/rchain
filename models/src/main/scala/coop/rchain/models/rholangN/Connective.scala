package coop.rchain.models.rholangN

/** Connective for type Bool in pattern */
final class ConnBoolN() extends ConnectiveSTypeN
object ConnBoolN { def apply(): ConnBoolN = new ConnBoolN }

/** Connective for type Int in pattern */
final class ConnIntN() extends ConnectiveSTypeN
object ConnIntN { def apply(): ConnIntN = new ConnIntN }

/** Connective for type BigInt in pattern */
final class ConnBigIntN() extends ConnectiveSTypeN
object ConnBigIntN { def apply(): ConnBigIntN = new ConnBigIntN }

/** Connective for type String in pattern */
final class ConnStringN() extends ConnectiveSTypeN
object ConnStringN { def apply(): ConnStringN = new ConnStringN }

/** Connective for type Uri in pattern */
final class ConnUriN() extends ConnectiveSTypeN
object ConnUriN { def apply(): ConnUriN = new ConnUriN }

/** Connective for type ByteArray in pattern */
final class ConnByteArrayN() extends ConnectiveSTypeN
object ConnByteArrayN { def apply(): ConnByteArrayN = new ConnByteArrayN }

/** The "~" (logical Not) for pattern matching.
  * the pattern ~p says "anything but p" */
final class ConnNotBodyN(val p: ParN) extends ConnectiveFuncN
object ConnNotBodyN { def apply(p: ParN): ConnNotBodyN = new ConnNotBodyN(p) }

/** The "/\" (logical And) Conjunction for pattern matching. */
// TODO: Consider a replacement `ps: Seq[ParN]` to `p1: ParN, p2: ParN`
final class ConnAndBodyN(val ps: Seq[ParN]) extends ConnectiveFuncN
object ConnAndBodyN {
  def apply(ps: Seq[ParN]): ConnAndBodyN      = new ConnAndBodyN(ps)
  def apply(p1: ParN, p2: ParN): ConnAndBodyN = new ConnAndBodyN(Seq(p1, p2))
}

/** The "\/" (logical Or) Disjunction for pattern matching. */
// TODO: Consider a replacement `ps: Seq[ParN]` to `p1: ParN, p2: ParN`
final class ConnOrBodyN(val ps: Seq[ParN]) extends ConnectiveFuncN
object ConnOrBodyN {
  def apply(ps: Seq[ParN]): ConnOrBodyN      = new ConnOrBodyN(ps)
  def apply(p1: ParN, p2: ParN): ConnOrBodyN = new ConnOrBodyN(Seq(p1, p2))
}

/** The "=..." Binding for Bound variable in pattern matching.
  * E.g. for(@{=*x} <- @Nil) { Nil } */
final class VarRefN(val index: Int, val depth: Int) extends ConnectiveVarN
object VarRefN { def apply(index: Int, depth: Int): VarRefN = new VarRefN(index, depth) }
