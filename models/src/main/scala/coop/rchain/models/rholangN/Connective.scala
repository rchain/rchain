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
final class ConnNotN(val p: ParN) extends ConnectiveFuncN
object ConnNotN { def apply(p: ParN): ConnNotN = new ConnNotN(p) }

/** The "/\" (logical And) Conjunction for pattern matching. */
// TODO: Consider a replacement `ps: Seq[ParN]` to `p1: ParN, p2: ParN`
final class ConnAndN(val ps: Seq[ParN]) extends ConnectiveFuncN
object ConnAndN {
  def apply(ps: Seq[ParN]): ConnAndN      = new ConnAndN(ps)
  def apply(p1: ParN, p2: ParN): ConnAndN = new ConnAndN(Seq(p1, p2))
}

/** The "\/" (logical Or) Disjunction for pattern matching. */
// TODO: Consider a replacement `ps: Seq[ParN]` to `p1: ParN, p2: ParN`
final class ConnOrN(val ps: Seq[ParN]) extends ConnectiveFuncN
object ConnOrN {
  def apply(ps: Seq[ParN]): ConnOrN      = new ConnOrN(ps)
  def apply(p1: ParN, p2: ParN): ConnOrN = new ConnOrN(Seq(p1, p2))
}

/** The "=..." Binding for Bound variable in pattern matching.
  * The purpose of VarRef is to provide a mechanism to bind variables to values or processes
  * within pattern matching structures in Rholang, which is useful for controlling the flow of information
  * and processes within a Rholang program.
  * E.g.:
  * match someProc { =x => x!(*someChannel) }
  *  or
  * for(@{=*x} <- someChannel) { x!(*someOtherChannel) }
  */
final class ConnVarRefN(val index: Int, val depth: Int) extends ConnectiveVarN
object ConnVarRefN {
  def apply(index: Int, depth: Int): ConnVarRefN = new ConnVarRefN(index, depth)
}
