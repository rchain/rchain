package coop.rchain.models.rholangN

final class EListN(val ps: Seq[ParN], val remainder: Option[VarN]) extends ExprN

object EListN {
  def apply(ps: Seq[ParN] = Seq(), r: Option[VarN] = None): EListN = new EListN(ps, r)
  def apply(p: ParN): EListN                                       = apply(Seq(p), None)
}
