package coop.rchain.models.rholangn.parmanager

import cats.Eval
import coop.rchain.models.rholangn._
import cats.syntax.all._

private[parmanager] object ConnectiveUsed {
  private def cUsed(p: RhoTypeN): Eval[Boolean] = p.connectiveUsed
  private def cUsed(kv: (RhoTypeN, RhoTypeN)): Eval[Boolean] =
    (cUsed(kv._1), cUsed(kv._2)).mapN(_ || _)
  private def cUsed(ps: Seq[RhoTypeN]): Eval[Boolean] = ps.existsM(cUsed)
  private def cUsedKVPairs(kVPairs: Seq[(RhoTypeN, RhoTypeN)]): Eval[Boolean] =
    kVPairs.existsM(cUsed)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def connectiveUsedFn(p: RhoTypeN): Eval[Boolean] = Eval.defer {
    p match {

      /** Basic types */
      case _: NilN.type      => Eval.False
      case pProc: ParProcN   => cUsed(pProc.ps)
      case send: SendN       => (cUsed(send.chan), cUsed(send.data)).mapN(_ || _)
      case receive: ReceiveN => (cUsed(receive.binds), cUsed(receive.body)).mapN(_ || _)
      case m: MatchN         => (cUsed(m.target), cUsed(m.cases)).mapN(_ || _)
      case _: NewN           => Eval.False // There are no situations when New gets into the matcher

      /** Ground types */
      case _: GroundN => Eval.False

      /** Collections */
      case eList: EListN   => (cUsed(eList.ps), eList.remainder.existsM(cUsed)).mapN(_ || _)
      case eTuple: ETupleN => cUsed(eTuple.ps)
      case eSet: ESetN     => (cUsed(eSet.sortedPs), eSet.remainder.existsM(cUsed)).mapN(_ || _)
      case eMap: EMapN     => (cUsedKVPairs(eMap.sortedPs), eMap.remainder.existsM(cUsed)).mapN(_ || _)

      /** Vars */
      case _: BoundVarN      => Eval.False
      case _: FreeVarN       => Eval.True
      case _: WildcardN.type => Eval.True

      /** Operations */
      case op: Operation1ParN  => cUsed(op.p)
      case op: Operation2ParN  => (cUsed(op.p1), cUsed(op.p2)).mapN(_ || _)
      case eMethod: EMethodN   => (cUsed(eMethod.target), cUsed(eMethod.arguments)).mapN(_ || _)
      case eMatches: EMatchesN => cUsed(eMatches.target)

      /** Unforgeable names */
      case _: UnforgeableN => Eval.False

      /** Connective */
      case _: ConnectiveSTypeN => Eval.True
      case _: ConnectiveFuncN  => Eval.True
      case _: ConnectiveVarN   => Eval.False

      /** Auxiliary types */
      case bind: ReceiveBindN => cUsed(bind.source)
      case mCase: MatchCaseN  => cUsed(mCase.source)

      /** Other types */
      case _: BundleN => Eval.False // There are no situations when New gets into the matcher

      case x => throw new Exception(s"Undefined type $x")
    }
  }
}
