package coop.rchain.models.rholangN

/** *
  * A receive is written `for(binds) { body }`
  * i.e. `for(patterns <- source) { body }`
  * or for a persistent recieve: `for(patterns <= source) { body }`.
  *
  * It's an error for free Variable to occur more than once in a pattern.
  */
final class ReceiveN(
    val binds: Seq[ReceiveBindN],
    val body: ParN,
    val persistent: Boolean,
    val peek: Boolean,
    val bindCount: Int
) extends ParN

object ReceiveN {
  def apply(
      binds: Seq[ReceiveBindN],
      body: ParN,
      persistent: Boolean,
      peek: Boolean,
      bindCount: Int
  ): ReceiveN =
    new ReceiveN(binds, body, persistent, peek, bindCount)

  def apply(
      bind: ReceiveBindN,
      body: ParN,
      persistent: Boolean,
      peek: Boolean,
      bindCount: Int
  ): ReceiveN =
    apply(Seq(bind), body, persistent, peek, bindCount)

  def apply(binds: Seq[ReceiveBindN], body: ParN, bindCount: Int): ReceiveN =
    apply(binds, body, persistent = false, peek = false, bindCount)

  def apply(bind: ReceiveBindN, body: ParN, bindCount: Int): ReceiveN =
    apply(Seq(bind), body, bindCount)
}

final class ReceiveBindN(
    val patterns: Seq[ParN],
    val source: ParN,
    val remainder: Option[VarN],
    val freeCount: Int
) extends AuxParN

object ReceiveBindN {
  def apply(
      patterns: Seq[ParN],
      source: ParN,
      remainder: Option[VarN],
      freeCount: Int
  ): ReceiveBindN = new ReceiveBindN(patterns, source, remainder, freeCount)

  def apply(pattern: ParN, source: ParN, remainder: Option[VarN], freeCount: Int): ReceiveBindN =
    apply(Seq(pattern), source, remainder, freeCount)

  def apply(patterns: Seq[ParN], source: ParN, freeCount: Int): ReceiveBindN =
    new ReceiveBindN(patterns, source, None, freeCount)

  def apply(pattern: ParN, source: ParN, freeCount: Int): ReceiveBindN =
    apply(Seq(pattern), source, freeCount)
}
