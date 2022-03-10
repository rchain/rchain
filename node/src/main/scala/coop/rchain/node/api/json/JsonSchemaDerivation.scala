package coop.rchain.node.api.json

import com.google.protobuf.ByteString
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{
  BlockInfo,
  BondInfo,
  DeployData,
  DeployInfo,
  JustificationInfo,
  LightBlockInfo,
  RejectedDeployInfo
}
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models.GUnforgeable.UnfInstance
import coop.rchain.models.Var.{VarInstance, WildcardMsg}
import coop.rchain.models._
import coop.rchain.models.syntax._
import coop.rchain.node.api.WebApi._
import coop.rchain.node.web.{VersionInfo, _}
import endpoints4s.{Invalid, Valid}

import scala.Function.const
import scala.collection.immutable.BitSet

/**
  * JsonSchema derivations for Web API request/response objects (Scala sealed traits and case classes)
  */
trait JsonSchemaDerivations extends JsonSchemaDerivationsBase {

  // format: off

  implicit lazy val deployDataSchema               : JsonSchema[DeployData]                   = schemaRecord
  implicit lazy val deployRequestSchema            : JsonSchema[DeployRequest]                = schemaRecord
  implicit lazy val versionInfoSchema              : JsonSchema[VersionInfo]                  = schemaRecord
  implicit lazy val apiStatusSchema                : JsonSchema[ApiStatus]                    = schemaRecord
  implicit lazy val exploreDeployReqSchema         : JsonSchema[ExploreDeployRequest]         = schemaRecord
  implicit lazy val dataAtNameByBlockHashReqSchema : JsonSchema[DataAtNameByBlockHashRequest] = schemaRecord
  implicit lazy val rhoResponseSchema              : JsonSchema[RhoDataResponse]              = schemaRecord
  implicit lazy val bondInfoSchema                 : JsonSchema[BondInfo]                     = schemaRecord
  implicit lazy val justInfoSchema                 : JsonSchema[JustificationInfo]            = schemaRecord
  implicit lazy val rejectedInfoSchema             : JsonSchema[RejectedDeployInfo]           = schemaRecord
  implicit lazy val lightBlockInfoSchema           : JsonSchema[LightBlockInfo]               = schemaRecord
  implicit lazy val deployInfoSchema               : JsonSchema[DeployInfo]                   = schemaRecord
  implicit lazy val blockInfoSchema                : JsonSchema[BlockInfo]                    = schemaRecord
  implicit lazy val transactionTypeSchema          : JsonSchema[TransactionType]              = schemaTagged
  implicit lazy val sysTranTypeSchema              : JsonSchema[SystemTransaction]            = schemaTagged
  implicit lazy val transactionSchema              : JsonSchema[Transaction]                  = schemaRecord
  implicit lazy val transactionInfoSchema          : JsonSchema[TransactionInfo]              = schemaRecord
  implicit lazy val transactionRespSchema          : JsonSchema[TransactionResponse]          = schemaRecord
  implicit lazy val prepareRespSchema              : JsonSchema[PrepareResponse]              = schemaRecord
  implicit lazy val prepareReqSchema               : JsonSchema[PrepareRequest]               = schemaRecord
  implicit lazy val dataAtNameReqSchema            : JsonSchema[DataAtNameRequest]            = schemaRecord
  implicit lazy val rhoExprWithBlockSchema         : JsonSchema[RhoExprWithBlock]             = schemaRecord
  implicit lazy val dataAtNameRespSchema           : JsonSchema[DataAtNameResponse]           = schemaRecord

  // Web API Rholang types (subset of protobuf generated types)
  implicit lazy val rhoExprSchema: JsonSchema[RhoExpr] =
    lazySchema("RhoExprRef")(schemaTagged[RhoExpr]).withDescription("Rholang expression (Par type)")
  // TODO: add ExprXXX types (not necessary for derivation but for short type name without a namespace)
  implicit lazy val rhoUnforgSchema     : JsonSchema[RhoUnforg]      = schemaTagged
  implicit lazy val unforgPrivateSchema : JsonSchema[UnforgPrivate]  = schemaRecord
  implicit lazy val unforgDeploySchema  : JsonSchema[UnforgDeploy]   = schemaRecord
  implicit lazy val unforgDeployerSchema: JsonSchema[UnforgDeployer] = schemaRecord

  /* Protobuf generated Rholang types (from models project) */

  // Par
  implicit lazy val parSchema: JsonSchema[Par] = lazySchema("ParRef")(schemaRecord[Par])

  // Send
  implicit lazy val sendSchema: JsonSchema[Send] = schemaRecord

  // Receive
  implicit lazy val wildcardMsgSchema: JsonSchema[WildcardMsg] = schemaRecord
  implicit lazy val varInstanceSchema: JsonSchema[VarInstance] = schemaTagged
  implicit lazy val varSchema        : JsonSchema[Var]         = schemaRecord
  // TODO: this is useless definition because in schema it only visible as `Option`
  implicit lazy val optionVarSchema  : JsonSchema[Option[Var]] = genericJsonSchema
  implicit lazy val receiveBindSchema: JsonSchema[ReceiveBind] = schemaRecord
  implicit lazy val receiveSchema    : JsonSchema[Receive]     = schemaRecord

  // New
  implicit lazy val newSchema: JsonSchema[New] = schemaRecord

  // Expr
  implicit lazy val eNotSchema  : JsonSchema[ENot]   = schemaRecord
  implicit lazy val eNegSchema  : JsonSchema[ENeg]   = schemaRecord
  implicit lazy val eMultSchema : JsonSchema[EMult]  = schemaRecord
  implicit lazy val eDivSchema  : JsonSchema[EDiv]   = schemaRecord
  implicit lazy val ePlusSchema : JsonSchema[EPlus]  = schemaRecord
  implicit lazy val eMinusSchema: JsonSchema[EMinus] = schemaRecord
  implicit lazy val eLtSchema   : JsonSchema[ELt]    = schemaRecord
  implicit lazy val eLteSchema  : JsonSchema[ELte]   = schemaRecord
  implicit lazy val eGtSchema   : JsonSchema[EGt]    = schemaRecord
  implicit lazy val eGteSchema  : JsonSchema[EGte]   = schemaRecord
  implicit lazy val eEqSchema   : JsonSchema[EEq]    = schemaRecord
  implicit lazy val eNeqSchema  : JsonSchema[ENeq]   = schemaRecord
  implicit lazy val eAndSchema  : JsonSchema[EAnd]   = schemaRecord
  implicit lazy val eOrSchema   : JsonSchema[EOr]    = schemaRecord
  implicit lazy val eVarSchema  : JsonSchema[EVar]   = schemaRecord
  implicit lazy val eListSchema : JsonSchema[EList]  = schemaRecord
  implicit lazy val eTupleSchema: JsonSchema[ETuple] = schemaRecord
  // Expr - Set, Map, method
  implicit lazy val eSetSchema   : JsonSchema[ESet]         = schemaRecord
  implicit lazy val kvPairSchema : JsonSchema[KeyValuePair] = schemaRecord
  implicit lazy val eMapSchema   : JsonSchema[EMap]         = schemaRecord
  implicit lazy val eMethodSchema: JsonSchema[EMethod]      = schemaRecord
  // Expr - match, binary ops
  implicit lazy val eMatchesSchema       : JsonSchema[EMatches]        = schemaRecord
  implicit lazy val ePercentPercentSchema: JsonSchema[EPercentPercent] = schemaRecord
  implicit lazy val ePlusPlusSchema      : JsonSchema[EPlusPlus]       = schemaRecord
  implicit lazy val eMinusMinusSchema    : JsonSchema[EMinusMinus]     = schemaRecord
  implicit lazy val eModSchema           : JsonSchema[EMod]            = schemaRecord
  implicit lazy val exprInstanceSchema   : JsonSchema[ExprInstance]    = schemaTagged
  implicit lazy val exprSchema           : JsonSchema[Expr]            = schemaRecord

  // Match
  implicit lazy val matchCaseSchema: JsonSchema[MatchCase] = schemaRecord
  implicit lazy val matchSchema    : JsonSchema[Match]     = schemaRecord

  // Unforgeable
  implicit lazy val gPrivateSchema     : JsonSchema[GPrivate]      = schemaRecord
  implicit lazy val gDeployIdSchema    : JsonSchema[GDeployId]     = schemaRecord
  implicit lazy val gDeployerIdSchema  : JsonSchema[GDeployerId]   = schemaRecord
  implicit lazy val gSysAuthTokenSchema: JsonSchema[GSysAuthToken] = schemaRecord
  implicit lazy val unfInstanceSchema  : JsonSchema[UnfInstance]   = schemaTagged
  implicit lazy val gUnforgeableSchema : JsonSchema[GUnforgeable]  = schemaRecord

  // Bundle
  implicit lazy val bundleSchema: JsonSchema[Bundle] = schemaRecord

  // Connective
  implicit lazy val varRefSchema            : JsonSchema[VarRef]             = schemaRecord
  implicit lazy val connectiveBodySchema    : JsonSchema[ConnectiveBody]     = schemaRecord
  implicit lazy val connectiveInstanceSchema: JsonSchema[ConnectiveInstance] = schemaTagged
  implicit lazy val connectiveSchema        : JsonSchema[Connective]         = schemaRecord

  // AlwaysEqual[BitSet]
  // TODO: this is useless definition because in schema it only visible as `AlwaysEqual`
  implicit lazy val alwaysEqualBitSetSchema: JsonSchema[AlwaysEqual[BitSet]] = genericJsonSchema

  // format: on

  // Json Schema for Rholang Set as Json array of Par's
  implicit lazy val parSetSchema: JsonSchema[ParSet] =
    arrayJsonSchema[List, Par].xmap(ParSet(_))(_.ps.sortedPars)

  // Json Schema for Rholang Map as Json object
  // TODO: solution for keys which cannot be converted to String
  implicit lazy val parMapSchema: JsonSchema[ParMap] =
    mapJsonSchema[Par].xmap { x =>
      import rholang.implicits._
      val seqParPar = x.toList.map(y => (GString(y._1): Par, y._2))
      ParMap(seqParPar)
    }(x => x.ps.toMap.map { case (k, v) => (k.toString, v) })

  // Json Schema encoding for ByteString as String base 16
  implicit lazy val byteStringSchema: JsonSchema[ByteString] =
    defaultStringJsonSchema.xmapPartial { str =>
      val bytesOpt = str.hexToByteString
      bytesOpt.map(Valid(_)).getOrElse(Invalid(s"Invalid hex format '$str'"))
    }(PrettyPrinter.buildStringNoLimit)

  // Json Schema for BitSet (dummy because it's not important for API)
  implicit lazy val bitSetSchema: JsonSchema[BitSet] =
    emptyRecord.xmap(const(BitSet()))(const(()))
}

/**
  * Helpers JSON schema derivation functions with short type name without a namespace.
  */
trait JsonSchemaDerivationsBase extends endpoints4s.generic.JsonSchemas {
  import scala.reflect.runtime.universe
  import scala.reflect.runtime.universe._

  def schemaRecord[T: TypeTag: GenericJsonSchema.GenericRecord]: JsonSchema[T] = {
    val tpeName = universe.typeOf[T].typeSymbol.name.decodedName.toString
    genericRecord[T].named(tpeName)
  }

  def schemaTagged[T: TypeTag: GenericJsonSchema.GenericTagged]: JsonSchema[T] = {
    val tpeName = universe.typeOf[T].typeSymbol.name.decodedName.toString
    genericTagged[T].named(tpeName)
  }
}
