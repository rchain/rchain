package coop.rchain.node.encode

import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{
  BondInfo,
  DeployChain,
  JustificationInfo,
  LightBlockInfo,
  RejectedDeployInfo,
  StateMetadata
}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.GUnforgeable.UnfInstance
import coop.rchain.models.Var.{VarInstance, WildcardMsg}
import coop.rchain.models._

import scala.collection.immutable.BitSet

object JsonEncoder {
  import io.circe._
  import io.circe.Encoder._
  import io.circe.generic.semiauto._

  implicit val encodeByteString: Encoder[ByteString] =
    Encoder.encodeString.contramap[ByteString](PrettyPrinter.buildStringNoLimit)
  implicit val encodeBondInfo: Encoder[BondInfo] = deriveEncoder[BondInfo]
  implicit val encodeJustificationInfo: Encoder[JustificationInfo] =
    deriveEncoder[JustificationInfo]
  implicit val encodeRejectedDeployInfo: Encoder[RejectedDeployInfo] =
    deriveEncoder[RejectedDeployInfo]
  implicit val encodeStateMetadata: Encoder[StateMetadata] =
    Encoder.encodeString.contramap[StateMetadata](s => s"""
         | proposed: ${s.proposed.map(v => PrettyPrinter.buildString(v.deploys)).mkString(" | ")}
         | accepted: ${s.acceptedSet.map(v => PrettyPrinter.buildString(v.deploys)).mkString(" | ")}
         | rejected: ${s.rejectedSet.map(v => PrettyPrinter.buildString(v.deploys)).mkString(" | ")}
         |""".stripMargin)
  implicit val encodeLightBlockInfo: Encoder[LightBlockInfo] = deriveEncoder[LightBlockInfo]
  implicit val encodePar: Encoder[Par]                       = deriveEncoder[Par]
  implicit val encodeSend: Encoder[Send]                     = deriveEncoder[Send]
  implicit val encodeWildcardMsg: Encoder[WildcardMsg]       = deriveEncoder[WildcardMsg]
  implicit val encodeVarInstance: Encoder[VarInstance]       = deriveEncoder[VarInstance]
  implicit val encodeVar: Encoder[Var]                       = deriveEncoder[Var]
  implicit val encodeReceiveBind: Encoder[ReceiveBind]       = deriveEncoder[ReceiveBind]
  implicit val encodeReceive: Encoder[Receive]               = deriveEncoder[Receive]
  implicit val encodeNew: Encoder[New]                       = deriveEncoder[New]
  implicit val encodeENot: Encoder[ENot]                     = deriveEncoder[ENot]
  implicit val encodeENeg: Encoder[ENeg]                     = deriveEncoder[ENeg]
  implicit val encodeEMult: Encoder[EMult]                   = deriveEncoder[EMult]
  implicit val encodeEDiv: Encoder[EDiv]                     = deriveEncoder[EDiv]
  implicit val encodeEPlus: Encoder[EPlus]                   = deriveEncoder[EPlus]
  implicit val encodeEMinus: Encoder[EMinus]                 = deriveEncoder[EMinus]
  implicit val encodeELt: Encoder[ELt]                       = deriveEncoder[ELt]
  implicit val encodeELte: Encoder[ELte]                     = deriveEncoder[ELte]
  implicit val encodeEGt: Encoder[EGt]                       = deriveEncoder[EGt]
  implicit val encodeEGte: Encoder[EGte]                     = deriveEncoder[EGte]
  implicit val encodeEEq: Encoder[EEq]                       = deriveEncoder[EEq]
  implicit val encodeENeq: Encoder[ENeq]                     = deriveEncoder[ENeq]
  implicit val encodeEAnd: Encoder[EAnd]                     = deriveEncoder[EAnd]
  implicit val encodeEOr: Encoder[EOr]                       = deriveEncoder[EOr]
  implicit val encodeEVar: Encoder[EVar]                     = deriveEncoder[EVar]
  implicit val encodeEList: Encoder[EList]                   = deriveEncoder[EList]
  implicit val encodeETuple: Encoder[ETuple]                 = deriveEncoder[ETuple]
  implicit val encodeParSet: Encoder[ParSet] =
    Encoder.encodeList[Par].contramapArray[ParSet](s => s.ps.iterator.toList)
  implicit val encodeParMap: Encoder[ParMap] =
    Encoder.encodeList[(Par, Par)].contramap[ParMap](m => m.ps.iterator.toList)
  implicit val encodeEMethod: Encoder[EMethod]                 = deriveEncoder[EMethod]
  implicit val encodeEMatches: Encoder[EMatches]               = deriveEncoder[EMatches]
  implicit val encodeEPercentPercent: Encoder[EPercentPercent] = deriveEncoder[EPercentPercent]
  implicit val encodeEPlusPlus: Encoder[EPlusPlus]             = deriveEncoder[EPlusPlus]
  implicit val encodeEMinusMinus: Encoder[EMinusMinus]         = deriveEncoder[EMinusMinus]
  implicit val encodeEMod: Encoder[EMod]                       = deriveEncoder[EMod]
  implicit val encodeExprInstance: Encoder[ExprInstance]       = deriveEncoder[ExprInstance]
  implicit val encodeExpr: Encoder[Expr]                       = deriveEncoder[Expr]
  implicit val encodeMatchCase: Encoder[MatchCase]             = deriveEncoder[MatchCase]
  implicit val encodeMatch: Encoder[Match]                     = deriveEncoder[Match]
  implicit val encodeGPrivate: Encoder[GPrivate]               = deriveEncoder[GPrivate]
  implicit val encodeGDeployId: Encoder[GDeployId]             = deriveEncoder[GDeployId]
  implicit val encodeGDeployerId: Encoder[GDeployerId]         = deriveEncoder[GDeployerId]
  implicit val encodeGSysAuthToken: Encoder[GSysAuthToken]     = deriveEncoder[GSysAuthToken]
  implicit val encodeUnfInstance: Encoder[UnfInstance]         = deriveEncoder[UnfInstance]
  implicit val encodeGUnforgeable: Encoder[GUnforgeable]       = deriveEncoder[GUnforgeable]
  implicit val encodeBundle: Encoder[Bundle]                   = deriveEncoder[Bundle]
  implicit val encodeVarRef: Encoder[VarRef]                   = deriveEncoder[VarRef]
  implicit val encodeConnectiveBody: Encoder[ConnectiveBody]   = deriveEncoder[ConnectiveBody]
  implicit val encodeConnectiveInstance: Encoder[ConnectiveInstance] =
    deriveEncoder[ConnectiveInstance]
  implicit val encodeConnective: Encoder[Connective] = deriveEncoder[Connective]
  implicit val encodeAlwaysEqual: Encoder[AlwaysEqual[BitSet]] =
    Encoder.encodeUnit.contramap[AlwaysEqual[BitSet]](_ => ())
  implicit val encodeBlake2b512Random: Encoder[Blake2b512Random] =
    Encoder.encodeUnit.contramap[Blake2b512Random](_ => ())
}
