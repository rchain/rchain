package coop.rchain.models.rholangn

import cats.Eval
import cats.syntax.all._
import coop.rchain.models.rholangn.ParN._
import coop.rchain.models.rholangn.parmanager.RhoHash

object NilN extends BasicN

/** *
  * Rholang process
  *
  * For example, `@0!(1) | @2!(3) | for(x <- @0) { Nil }` has two sends
  * and one receive.
  */
final class ParProcN(val ps: Seq[ParN]) extends BasicN {
  // Sorted by objects hash which is memoized as Rho type
  val psSorted: Eval[Seq[ParN]] =
    this.ps.traverse(p => p.rhoHash.map((p, _))).map(_.sortBy(_._2).map(_._1)).memoize
}

object ParProcN { def apply(ps: Seq[ParN]): ParProcN = new ParProcN(ps) }

/** *
  * A send is written `chan!(data)` or `chan!!(data)` for a persistent send.
  * Upon send, all free variables in data are substituted with their values.
  */
final class SendN(val chan: ParN, val args: Seq[ParN], val persistent: Boolean) extends BasicN
object SendN {
  def apply(chan: ParN, args: Seq[ParN], persistent: Boolean): SendN =
    new SendN(chan, args, persistent)

  def apply(chan: ParN, args: Seq[ParN]): SendN =
    apply(chan, args, persistent = false)

  def apply(chan: ParN, args: ParN, persistent: Boolean): SendN =
    apply(chan, Seq(args), persistent)

  def apply(chan: ParN, arg: ParN): SendN =
    apply(chan, Seq(arg), persistent = false)
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
  // Sorted by objects hash which is memoized as Rho type
  val bindsSorted: Eval[Seq[ReceiveBindN]] =
    this.binds.traverse(rb => rb.rhoHash.map((rb, _))).map(_.sortBy(_._2).map(_._1)).memoize
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
) {

  /** Cryptographic hash code of this object */
  val rhoHash: Eval[Array[Byte]] = RhoHash.hashReceiveBind(this).memoize
}

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

final class MatchN(val target: ParN, val cases: Seq[MatchCaseN]) extends BasicN

object MatchN {
  def apply(target: ParN, cases: Seq[MatchCaseN]): MatchN = new MatchN(target, cases)
  def apply(target: ParN, mCase: MatchCaseN): MatchN      = apply(target, Seq(mCase))
}

final class MatchCaseN(val pattern: ParN, val source: ParN, val freeCount: Int) {

  /** Cryptographic hash code of this object */
  val rhoHash: Eval[Array[Byte]] = RhoHash.hashMatchCase(this).memoize
}

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
    val uri: Seq[GStringN],
    val injections: Map[GStringN, ParN]
) extends BasicN {
  // Sorted by objects hash which is memoized as Rho type
  val urisSorted: Eval[Seq[GStringN]] =
    this.uri.traverse(uri => uri.rhoHash.map((uri, _))).map(_.sortBy(_._2).map(_._1)).memoize

  // Sorted by objects hash which is memoized as Rho type
  val injectionsSorted: Eval[Seq[(GStringN, ParN)]] =
    this.injections.toSeq
      .traverse(inj => inj.bimap(_.rhoHash, _.rhoHash).mapN(_ ++ _).map((inj, _)))
      .map(_.sortBy(_._2).map(_._1))
      .memoize

  def injectionsStrKeys: Map[String, ParN] = this.injections.map(_.bimap(_.v, identity))
}

object NewN {
  def apply(
      bindCount: Int,
      p: ParN,
      uri: Seq[String],
      injections: Map[String, ParN]
  ): NewN =
    new NewN(bindCount, p, uri.map(GStringN(_)), injections.map(_.bimap(GStringN(_), identity)))

  def apply(
      bindCount: Int,
      p: ParN,
      uri: Seq[GStringN],
      injections: Seq[(GStringN, ParN)]
  ): NewN = new NewN(bindCount, p, uri, injections.toMap)

  def apply(bindCount: Int, p: ParN): NewN = new NewN(bindCount, p, Seq(), Map())
}
