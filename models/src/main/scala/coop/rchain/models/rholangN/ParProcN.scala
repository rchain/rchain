package coop.rchain.models.rholangN

/** *
 * Rholang process
 *
 * For example, `@0!(1) | @2!(3) | for(x <- @0) { Nil }` has two sends
 * and one receive.
 */

final class ParProcN(val ps: Seq[ParN]) extends ParN {
  def add(p: ParN): ParProcN = ParProcN(ps :+ p)
}

object ParProcN {
  def apply(ps: Seq[ParN] = Seq()): ParProcN = new ParProcN(ps)
  def apply(p: ParN): ParProcN               = apply(Seq(p))
}
