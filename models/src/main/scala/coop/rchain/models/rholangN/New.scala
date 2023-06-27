package coop.rchain.models.rholangN

/**
  * The new construct serves as a variable binder with scope Proc which producesan unforgeable process
  * for each uniquely declared variable and substitutes these (quoted) processes for the variables.
  *
  * @param bindCount Total number of variables entered in p. This makes it easier to substitute or walk a term.
  * @param p Rholang executable code inside New.
  *        For normalized form, p should not contain solely another new.
  *        Also for normalized form, the first use should be level+0, next use level+1
  *        up to level+count for the last used variable.
  * @param uri List of names Rho built-in processes listening on channels (e.g. `rho:io:stdout`).
  *        For normalization, uri-referenced variables come at the end, and in lexicographical order.
  */
final class NewN(val bindCount: Int, val p: ParN, val uri: Seq[String]) extends ParN

object NewN {
  def apply(bindCount: Int, p: ParN, uri: Seq[String] = Seq()): NewN = new NewN(bindCount, p, uri)
}
