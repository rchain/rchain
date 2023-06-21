package coop.rchain.models.rholangN

/**
  *
  */
final class MatchN(val target: ParN, val cases: Seq[MatchCaseN]) extends ParN

object MatchN {
  def apply(target: ParN, cases: Seq[MatchCaseN]): MatchN = new MatchN(target, cases)
  def apply(target: ParN, mCase: MatchCaseN): MatchN      = apply(target, Seq(mCase))
}

final class MatchCaseN(val pattern: ParN, val source: ParN, val freeCount: Int) extends AuxParN

object MatchCaseN {
  def apply(pattern: ParN, source: ParN, freeCount: Int): MatchCaseN =
    new MatchCaseN(pattern, source, freeCount)
}
