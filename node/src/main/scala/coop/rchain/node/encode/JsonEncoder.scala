package coop.rchain.node.encode

import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{BondInfo, JustificationInfo, LightBlockInfo, RejectedDeployInfo}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.GUnforgeable.UnfInstance
import coop.rchain.models.Var.{VarInstance, WildcardMsg}
import coop.rchain.models._
import io.circe.parser._
import cats.syntax.all._
import coop.rchain.shared.Base16
import coop.rchain.models.syntax._

import scala.collection.immutable.BitSet
import scodec.{Attempt, Err, Codec => SCodec}
import scodec.codecs.utf8

object JsonEncoder {
  import io.circe._
  import io.circe.Encoder._
  import io.circe.Decoder._
  import io.circe.generic.semiauto._

  implicit val encodeByteString: Encoder[ByteString] =
    Encoder.encodeString.contramap[ByteString](PrettyPrinter.buildStringNoLimit)
  implicit val encodeBondInfo: Encoder[BondInfo] = deriveEncoder[BondInfo]
  implicit val encodeJustificationInfo: Encoder[JustificationInfo] =
    deriveEncoder[JustificationInfo]
  implicit val encodeRejectedDeployInfo: Encoder[RejectedDeployInfo] =
    deriveEncoder[RejectedDeployInfo]
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
  implicit val encodeEShortAnd: Encoder[EShortAnd]           = deriveEncoder[EShortAnd]
  implicit val encodeEShortOr: Encoder[EShortOr]             = deriveEncoder[EShortOr]
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

  // FIXME blake2b512Random encode. Question Is that really neccessary?
  implicit val encodeBlake2b512Random: Encoder[Blake2b512Random] =
    Encoder.encodeUnit.contramap[Blake2b512Random](_ => ())

  implicit val decodeByteString: Decoder[ByteString] =
    Decoder.decodeString.map[ByteString](s => s.unsafeHexToByteString)
  implicit val decodeBondInfo: Decoder[BondInfo] = deriveDecoder[BondInfo]
  implicit val decodeJustificationInfo: Decoder[JustificationInfo] =
    deriveDecoder[JustificationInfo]
  implicit val decodeRejectedDeployInfo: Decoder[RejectedDeployInfo] =
    deriveDecoder[RejectedDeployInfo]
  implicit val decodeLightBlockInfo: Decoder[LightBlockInfo] = deriveDecoder[LightBlockInfo]
  implicit val decodePar: Decoder[Par]                       = deriveDecoder[Par]
  implicit val decodeSend: Decoder[Send]                     = deriveDecoder[Send]
  implicit val decodeWildcardMsg: Decoder[WildcardMsg]       = deriveDecoder[WildcardMsg]
  implicit val decodeVarInstance: Decoder[VarInstance]       = deriveDecoder[VarInstance]
  implicit val decodeVar: Decoder[Var]                       = deriveDecoder[Var]
  implicit val decodeReceiveBind: Decoder[ReceiveBind]       = deriveDecoder[ReceiveBind]
  implicit val decodeReceive: Decoder[Receive]               = deriveDecoder[Receive]
  implicit val decodeNew: Decoder[New]                       = deriveDecoder[New]
  implicit val decodeENot: Decoder[ENot]                     = deriveDecoder[ENot]
  implicit val decodeENeg: Decoder[ENeg]                     = deriveDecoder[ENeg]
  implicit val decodeEMult: Decoder[EMult]                   = deriveDecoder[EMult]
  implicit val decodeEDiv: Decoder[EDiv]                     = deriveDecoder[EDiv]
  implicit val decodeEPlus: Decoder[EPlus]                   = deriveDecoder[EPlus]
  implicit val decodeEMinus: Decoder[EMinus]                 = deriveDecoder[EMinus]
  implicit val decodeELt: Decoder[ELt]                       = deriveDecoder[ELt]
  implicit val decodeELte: Decoder[ELte]                     = deriveDecoder[ELte]
  implicit val decodeEGt: Decoder[EGt]                       = deriveDecoder[EGt]
  implicit val decodeEGte: Decoder[EGte]                     = deriveDecoder[EGte]
  implicit val decodeEEq: Decoder[EEq]                       = deriveDecoder[EEq]
  implicit val decodeENeq: Decoder[ENeq]                     = deriveDecoder[ENeq]
  implicit val decodeEAnd: Decoder[EAnd]                     = deriveDecoder[EAnd]
  implicit val decodeEOr: Decoder[EOr]                       = deriveDecoder[EOr]
  implicit val decodeEShortAnd: Decoder[EShortAnd]           = deriveDecoder[EShortAnd]
  implicit val decodeEShortOr: Decoder[EShortOr]             = deriveDecoder[EShortOr]
  implicit val decodeEVar: Decoder[EVar]                     = deriveDecoder[EVar]
  implicit val decodeEList: Decoder[EList]                   = deriveDecoder[EList]
  implicit val decodeETuple: Decoder[ETuple]                 = deriveDecoder[ETuple]
  implicit val decodeParSet: Decoder[ParSet] =
    Decoder.decodeList[Par].map[ParSet](p => ParSet(p, None))
  implicit val decodeParMap: Decoder[ParMap] =
    Decoder.decodeList[(Par, Par)].map[ParMap](m => ParMap(m))
  implicit val decodeEMethod: Decoder[EMethod]                 = deriveDecoder[EMethod]
  implicit val decodeEMatches: Decoder[EMatches]               = deriveDecoder[EMatches]
  implicit val decodeEPercentPercent: Decoder[EPercentPercent] = deriveDecoder[EPercentPercent]
  implicit val decodeEPlusPlus: Decoder[EPlusPlus]             = deriveDecoder[EPlusPlus]
  implicit val decodeEMinusMinus: Decoder[EMinusMinus]         = deriveDecoder[EMinusMinus]
  implicit val decodeEMod: Decoder[EMod]                       = deriveDecoder[EMod]
  implicit val decodeExprInstance: Decoder[ExprInstance]       = deriveDecoder[ExprInstance]
  implicit val decodeExpr: Decoder[Expr]                       = deriveDecoder[Expr]
  implicit val decodeMatchCase: Decoder[MatchCase]             = deriveDecoder[MatchCase]
  implicit val decodeMatch: Decoder[Match]                     = deriveDecoder[Match]
  implicit val decodeGPrivate: Decoder[GPrivate]               = deriveDecoder[GPrivate]
  implicit val decodeGDeployId: Decoder[GDeployId]             = deriveDecoder[GDeployId]
  implicit val decodeGDeployerId: Decoder[GDeployerId]         = deriveDecoder[GDeployerId]
  implicit val decodeGSysAuthToken: Decoder[GSysAuthToken]     = deriveDecoder[GSysAuthToken]
  implicit val decodeUnfInstance: Decoder[UnfInstance]         = deriveDecoder[UnfInstance]
  implicit val decodeGUnforgeable: Decoder[GUnforgeable]       = deriveDecoder[GUnforgeable]
  implicit val decodeBundle: Decoder[Bundle]                   = deriveDecoder[Bundle]
  implicit val decodeVarRef: Decoder[VarRef]                   = deriveDecoder[VarRef]
  implicit val decodeConnectiveBody: Decoder[ConnectiveBody]   = deriveDecoder[ConnectiveBody]
  implicit val decodeConnectiveInstance: Decoder[ConnectiveInstance] =
    deriveDecoder[ConnectiveInstance]
  implicit val decodeConnective: Decoder[Connective] = deriveDecoder[Connective]
  implicit val decodeAlwaysEqual: Decoder[AlwaysEqual[BitSet]] =
    Decoder.decodeUnit.map[AlwaysEqual[BitSet]](_ => AlwaysEqual(BitSet()))

  // FIXME blake2b512Random decode. Question Is that really neccessary?
  implicit val decodeDummyBlake2b512Random: Decoder[Blake2b512Random] =
    Decoder.decodeUnit.map[Blake2b512Random](_ => Blake2b512Random.defaultRandom)
}
