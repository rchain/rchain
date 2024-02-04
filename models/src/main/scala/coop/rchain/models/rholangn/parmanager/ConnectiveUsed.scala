package coop.rchain.models.rholangn.parmanager

import cats.Eval
import cats.syntax.all._
import coop.rchain.models.rholangn._

object ConnectiveUsed {
  def cUsed(p: RhoTypeN): Eval[Boolean]                               = p.connectiveUsed
  def cUsed(kv: (RhoTypeN, RhoTypeN)): Eval[Boolean]                  = (cUsed(kv._1), cUsed(kv._2)).mapN(_ || _)
  def cUsed(ps: Seq[RhoTypeN]): Eval[Boolean]                         = ps.existsM(cUsed)
  def cUsedKVPairs(kVPairs: Seq[(RhoTypeN, RhoTypeN)]): Eval[Boolean] = kVPairs.existsM(cUsed)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def connectiveUsedFn(input: RhoTypeN): Eval[Boolean] = Eval.defer {
    input match {

      /** Basic types */
      case _: NilN.type => Eval.False
      case p: ParProcN  => cUsed(p.ps)
      case p: SendN     => (cUsed(p.chan), cUsed(p.args)).mapN(_ || _)
      case p: ReceiveN  => (cUsed(p.binds.map(_.source)), cUsed(p.body)).mapN(_ || _)
      case p: MatchN    => (cUsed(p.target), cUsed(p.cases.map(_.source))).mapN(_ || _)
      case _: NewN      => Eval.False // There are no situations when New gets into the matcher

      /** Ground types */
      case _: GroundN => Eval.False

      /** Collections */
      case p: EListN  => (cUsed(p.ps), p.remainder.existsM(cUsed)).mapN(_ || _)
      case p: ETupleN => cUsed(p.ps)
      case p: ESetN   => (cUsed(p.ps.toSeq), p.remainder.existsM(cUsed)).mapN(_ || _)
      case p: EMapN   => (cUsedKVPairs(p.ps.toSeq), p.remainder.existsM(cUsed)).mapN(_ || _)

      /** Vars */
      case _: BoundVarN      => Eval.False
      case _: FreeVarN       => Eval.True
      case _: WildcardN.type => Eval.True

      /** Operations */
      case p: Operation1ParN => cUsed(p.p)
      case p: Operation2ParN => (cUsed(p.p1), cUsed(p.p2)).mapN(_ || _)
      case p: EMethodN       => (cUsed(p.target), cUsed(p.args)).mapN(_ || _)
      case p: EMatchesN      => cUsed(p.target)

      /** Unforgeable names */
      case _: UnforgeableN => Eval.False

      /** Connective */
      case _: ConnectiveSTypeN => Eval.True
      case _: ConnectiveFuncN  => Eval.True
      case _: ConnectiveVarN   => Eval.False

      /** Other types */
      case _: BundleN => Eval.False // There are no situations when New gets into the matcher

      case p => throw new Exception(s"Undefined type $p")
    }
  }
}
