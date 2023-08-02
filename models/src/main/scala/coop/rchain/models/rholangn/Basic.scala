package coop.rchain.models.rholangn

final class NilN() extends BasicN
object NilN { def apply(): NilN = new NilN }

/** *
  * Rholang process
  *
  * For example, `@0!(1) | @2!(3) | for(x <- @0) { Nil }` has two sends
  * and one receive.
  */
final class ParProcN(val ps: Seq[ParN]) extends BasicN {
  def sortedPs: Seq[ParN] = parmanager.Manager.sortPars(ps)
}
object ParProcN { def apply(ps: Seq[ParN]): ParProcN = new ParProcN(ps) }

/** *
  * A send is written `chan!(data)` or `chan!!(data)` for a persistent send.
  * Upon send, all free variables in data are substituted with their values.
  */
final class SendN(val chan: ParN, val data: Seq[ParN], val persistent: Boolean) extends BasicN
object SendN {
  def apply(chan: ParN, data: Seq[ParN], persistent: Boolean): SendN =
    new SendN(chan, data, persistent)

  def apply(chan: ParN, data: Seq[ParN]): SendN =
    apply(chan, data, persistent = false)

  def apply(chan: ParN, data: ParN, persistent: Boolean): SendN =
    apply(chan, Seq(data), persistent)

  def apply(chan: ParN, data: ParN): SendN =
    apply(chan, Seq(data), persistent = false)
}

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
) extends BasicN {
  def sortedBinds: Seq[ReceiveBindN] = parmanager.Manager.sortBinds(binds)
}
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

  def apply(pattern: ParN, source: ParN): ReceiveBindN =
    apply(Seq(pattern), source, 0)
}

/**
  *
  */
final class MatchN(val target: ParN, val cases: Seq[MatchCaseN]) extends BasicN

object MatchN {
  def apply(target: ParN, cases: Seq[MatchCaseN]): MatchN = new MatchN(target, cases)
  def apply(target: ParN, mCase: MatchCaseN): MatchN      = apply(target, Seq(mCase))
}

final class MatchCaseN(val pattern: ParN, val source: ParN, val freeCount: Int) extends AuxParN

object MatchCaseN {
  def apply(pattern: ParN, source: ParN, freeCount: Int = 0): MatchCaseN =
    new MatchCaseN(pattern, source, freeCount)
}

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
  * @param injections List of injected uri-referenced variables (e.g. rho:rchain:deployId).
  *        Should be sort by key in lexicographical order.
  */
final class NewN(
    val bindCount: Int,
    val p: ParN,
    val uri: Seq[String],
    val injections: Map[String, ParN]
) extends BasicN {
  def sortedUri: Seq[String]                = parmanager.Manager.sortUris(uri)
  def sortedInjections: Seq[(String, ParN)] = parmanager.Manager.sortInjections(injections)
}

object NewN {
  def apply(
      bindCount: Int,
      p: ParN,
      uri: Seq[String],
      injections: Map[String, ParN]
  ): NewN = new NewN(bindCount, p, uri, injections)

  def apply(
      bindCount: Int,
      p: ParN,
      uri: Seq[String],
      injections: Seq[(String, ParN)]
  ): NewN = new NewN(bindCount, p, uri, Map.from(injections))

  def apply(bindCount: Int, p: ParN): NewN = new NewN(bindCount, p, Seq(), Map())
}
