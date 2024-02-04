package coop.rchain.models.rholangn

final class ENegN(private val input: ParN) extends Operation1ParN {
  override val p: ParN = input
}
object ENegN { def apply(p: ParN): ENegN = new ENegN(p) }

final class ENotN(private val input: ParN) extends Operation1ParN {
  override val p: ParN = input
}
object ENotN { def apply(p: ParN): ENotN = new ENotN(p) }

final class EPlusN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EPlusN { def apply(p1: ParN, p2: ParN): EPlusN = new EPlusN(p1, p2) }

final class EMinusN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EMinusN { def apply(p1: ParN, p2: ParN): EMinusN = new EMinusN(p1, p2) }

final class EMultN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EMultN { def apply(p1: ParN, p2: ParN): EMultN = new EMultN(p1, p2) }

final class EDivN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EDivN { def apply(p1: ParN, p2: ParN): EDivN = new EDivN(p1, p2) }

final class EModN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EModN { def apply(p1: ParN, p2: ParN): EModN = new EModN(p1, p2) }

final class ELtN(val p1: ParN, val p2: ParN) extends Operation2ParN
object ELtN { def apply(p1: ParN, p2: ParN): ELtN = new ELtN(p1, p2) }

final class ELteN(val p1: ParN, val p2: ParN) extends Operation2ParN
object ELteN { def apply(p1: ParN, p2: ParN): ELteN = new ELteN(p1, p2) }

final class EGtN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EGtN { def apply(p1: ParN, p2: ParN): EGtN = new EGtN(p1, p2) }

final class EGteN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EGteN { def apply(p1: ParN, p2: ParN): EGteN = new EGteN(p1, p2) }

final class EEqN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EEqN { def apply(p1: ParN, p2: ParN): EEqN = new EEqN(p1, p2) }

final class ENeqN(val p1: ParN, val p2: ParN) extends Operation2ParN
object ENeqN { def apply(p1: ParN, p2: ParN): ENeqN = new ENeqN(p1, p2) }

final class EAndN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EAndN { def apply(p1: ParN, p2: ParN): EAndN = new EAndN(p1, p2) }

final class EShortAndN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EShortAndN { def apply(p1: ParN, p2: ParN): EShortAndN = new EShortAndN(p1, p2) }

final class EOrN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EOrN { def apply(p1: ParN, p2: ParN): EOrN = new EOrN(p1, p2) }

final class EShortOrN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EShortOrN { def apply(p1: ParN, p2: ParN): EShortOrN = new EShortOrN(p1, p2) }

final class EPlusPlusN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EPlusPlusN { def apply(p1: ParN, p2: ParN): EPlusPlusN = new EPlusPlusN(p1, p2) }

final class EMinusMinusN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EMinusMinusN { def apply(p1: ParN, p2: ParN): EMinusMinusN = new EMinusMinusN(p1, p2) }

final class EPercentPercentN(val p1: ParN, val p2: ParN) extends Operation2ParN
object EPercentPercentN {
  def apply(p1: ParN, p2: ParN): EPercentPercentN = new EPercentPercentN(p1, p2)
}

final class EMethodN(val target: ParN, val methodName: String, val args: Seq[ParN])
    extends OperationOtherN

object EMethodN {
  def apply(target: ParN, methodName: String, args: Seq[ParN] = Seq()): EMethodN =
    new EMethodN(target, methodName, args)
  def apply(target: ParN, methodName: String, arg: ParN): EMethodN =
    new EMethodN(target, methodName, Seq(arg))
}

/**
  * The p matches q expression is similar to:
  * match p { q -> true; _ -> false }
  */
final class EMatchesN(val target: ParN, val pattern: ParN) extends OperationOtherN
object EMatchesN {
  def apply(target: ParN, pattern: ParN): EMatchesN = new EMatchesN(target, pattern)
}
