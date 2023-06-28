package coop.rchain.models.rholangN

final class ENegN(private val input: ParN) extends Operation1ParN {
  override val p: ParN = input
}
object ENegN { def apply(p: ParN): ENegN = new ENegN(p) }

final class ENotN(private val input: ParN) extends Operation1ParN {
  override val p: ParN = input
}
object ENotN { def apply(p: ParN): ENotN = new ENotN(p) }

final class EPlusN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EPlusN { def apply(p1: ParN, p2: ParN): EPlusN = new EPlusN(p1, p2) }

final class EMinusN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EMinusN { def apply(p1: ParN, p2: ParN): EMinusN = new EMinusN(p1, p2) }

final class EMultN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EMultN { def apply(p1: ParN, p2: ParN): EMultN = new EMultN(p1, p2) }

final class EDivN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EDivN { def apply(p1: ParN, p2: ParN): EDivN = new EDivN(p1, p2) }

final class EModN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EModN { def apply(p1: ParN, p2: ParN): EModN = new EModN(p1, p2) }

final class ELtN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object ELtN { def apply(p1: ParN, p2: ParN): ELtN = new ELtN(p1, p2) }

final class ELteN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object ELteN { def apply(p1: ParN, p2: ParN): ELteN = new ELteN(p1, p2) }

final class EGtN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EGtN { def apply(p1: ParN, p2: ParN): EGtN = new EGtN(p1, p2) }

final class EGteN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EGteN { def apply(p1: ParN, p2: ParN): EGteN = new EGteN(p1, p2) }

final class EEqN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EEqN { def apply(p1: ParN, p2: ParN): EEqN = new EEqN(p1, p2) }

final class ENeqN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object ENeqN { def apply(p1: ParN, p2: ParN): ENeqN = new ENeqN(p1, p2) }

final class EAndN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EAndN { def apply(p1: ParN, p2: ParN): EAndN = new EAndN(p1, p2) }

final class EShortAndN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EShortAndN { def apply(p1: ParN, p2: ParN): EShortAndN = new EShortAndN(p1, p2) }

final class EOrN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EOrN { def apply(p1: ParN, p2: ParN): EOrN = new EOrN(p1, p2) }

final class EShortOrN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EShortOrN { def apply(p1: ParN, p2: ParN): EShortOrN = new EShortOrN(p1, p2) }

final class EPlusPlusN(private val input1: ParN, private val input2: ParN) extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EPlusPlusN { def apply(p1: ParN, p2: ParN): EPlusPlusN = new EPlusPlusN(p1, p2) }

final class EMinusMinusN(private val input1: ParN, private val input2: ParN)
    extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EMinusMinusN { def apply(p1: ParN, p2: ParN): EMinusMinusN = new EMinusMinusN(p1, p2) }

final class EPercentPercentN(private val input1: ParN, private val input2: ParN)
    extends Operation2ParN {
  override val p1: ParN = input1
  override val p2: ParN = input2
}
object EPercentPercentN {
  def apply(p1: ParN, p2: ParN): EPercentPercentN = new EPercentPercentN(p1, p2)
}

final class EMethodN(val methodName: String, val target: ParN, val arguments: Seq[ParN])
    extends OperationOtherN
object EMethodN {
  def apply(methodName: String, target: ParN, arguments: Seq[ParN] = Seq()): EMethodN =
    new EMethodN(methodName, target, arguments)
  def apply(methodName: String, target: ParN, argument: ParN): EMethodN =
    new EMethodN(methodName, target, Seq(argument))
}

/**
  * The p matches q expression is similar to:
  * match p { q -> true; _ -> false }
  */
final class EMatchesN(val target: ParN, val pattern: ParN) extends OperationOtherN
object EMatchesN {
  def apply(target: ParN, pattern: ParN): EMatchesN = new EMatchesN(target, pattern)
}
