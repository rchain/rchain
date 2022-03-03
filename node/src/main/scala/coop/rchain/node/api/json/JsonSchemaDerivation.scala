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
import coop.rchain.models.GUnforgeable.UnfInstance
import coop.rchain.models.Var.{VarInstance, WildcardMsg}
import coop.rchain.models.syntax._
import coop.rchain.models._
import coop.rchain.node.api.WebApi._
import endpoints4s.{Invalid, Valid}
import monix.eval.Coeval

import scala.collection.immutable.{BitSet, HashMap, HashSet}

/**
  * JsonSchema derivations for Web API request/response objects (Scala sealed traits and case classes)
  */
trait JsonSchemaDerivations extends JsonSchemaDerivationsBase {

  // format: off

  implicit lazy val deployDataSchema      : JsonSchema[DeployData]                   = schemaRecord
  implicit lazy val deployRequestSchema   : JsonSchema[DeployRequest]                = schemaRecord
  implicit lazy val versionInfoSchema     : JsonSchema[VersionInfo]                  = schemaRecord
  implicit lazy val apiStatusSchema       : JsonSchema[ApiStatus]                    = schemaRecord
  implicit lazy val exploreDeployReqSchema: JsonSchema[ExploreDeployRequest]         = schemaRecord
  implicit lazy val dataAtNameReqSchema   : JsonSchema[DataAtNameByBlockHashRequest] = schemaRecord
  implicit lazy val rhoResponseSchema     : JsonSchema[RhoDataResponse]              = schemaRecord
  implicit lazy val bondInfoSchema        : JsonSchema[BondInfo]                     = schemaRecord
  implicit lazy val justInfoSchema        : JsonSchema[JustificationInfo]            = schemaRecord
  implicit lazy val rejectedInfoSchema    : JsonSchema[RejectedDeployInfo]           = schemaRecord
  implicit lazy val lightBlockInfoSchema  : JsonSchema[LightBlockInfo]               = schemaRecord
  implicit lazy val deployInfoSchema      : JsonSchema[DeployInfo]                   = schemaRecord
  implicit lazy val blockInfoSchema       : JsonSchema[BlockInfo]                    = schemaRecord
//  implicit lazy val transactionInfoSchema : JsonSchema[TransactionInfo]              = schemaRecord

  // Web API Rholang types (subset of protobuf generated types)
  implicit lazy val rhoExprSchema: JsonSchema[RhoExpr] =
    lazySchema("RhoExprRef")(schemaTagged[RhoExpr]).withDescription("Rholang expression (Par type)")
  // TODO: add ExprXXX types (not necessary for derivation but for short type name without a namespace)
  implicit lazy val rhoUnforgSchema     : JsonSchema[RhoUnforg]      = schemaTagged
  implicit lazy val unforgPrivateSchema : JsonSchema[UnforgPrivate]  = schemaRecord
  implicit lazy val unforgDeploySchema  : JsonSchema[UnforgDeploy]   = schemaRecord
  implicit lazy val unforgDeployerSchema: JsonSchema[UnforgDeployer] = schemaRecord

  // Protobuf generated Rholang types (from models project)
//  implicit lazy val parSchema: JsonSchema[Par] = lazySchema("ParRef", schemaRecord[Par])
  /** TODO: add all referenced types in Par type, see [[coop.rchain.node.encode.JsonEncoder]] as example for circe derivations */
  // Par
  implicit lazy val parSchema: JsonSchema[Par] = schemaRecord[Par]

  // Send
  implicit val sendSchema: JsonSchema[Send] = schemaRecord[Send]

  // Receive
  implicit val wildcardMsgSchema: JsonSchema[WildcardMsg] = schemaRecord[WildcardMsg]
  implicit val varInstanceSchema: JsonSchema[VarInstance] = schemaTagged[VarInstance]
  implicit val varSchema        : JsonSchema[Var]         = schemaRecord[Var]
  implicit val receiveBindSchema: JsonSchema[ReceiveBind] = schemaRecord[ReceiveBind]
  implicit val receiveSchema    : JsonSchema[Receive]     = schemaRecord[Receive]

  // New
  implicit val newSchema: JsonSchema[New] = schemaRecord[New]

  // Expr
  implicit val eNotSchema  : JsonSchema[ENot]   = schemaRecord[ENot]
  implicit val eNegSchema  : JsonSchema[ENeg]   = schemaRecord[ENeg]
  implicit val eMultSchema : JsonSchema[EMult]  = schemaRecord[EMult]
  implicit val eDivSchema  : JsonSchema[EDiv]   = schemaRecord[EDiv]
  implicit val ePlusSchema : JsonSchema[EPlus]  = schemaRecord[EPlus]
  implicit val eMinusSchema: JsonSchema[EMinus] = schemaRecord[EMinus]
  implicit val eLtSchema   : JsonSchema[ELt]    = schemaRecord[ELt]
  implicit val eLteSchema  : JsonSchema[ELte]   = schemaRecord[ELte]
  implicit val eGtSchema   : JsonSchema[EGt]    = schemaRecord[EGt]
  implicit val eGteSchema  : JsonSchema[EGte]   = schemaRecord[EGte]
  implicit val eEqSchema   : JsonSchema[EEq]    = schemaRecord[EEq]
  implicit val eNeqSchema  : JsonSchema[ENeq]   = schemaRecord[ENeq]
  implicit val eAndSchema  : JsonSchema[EAnd]   = schemaRecord[EAnd]
  implicit val eOrSchema   : JsonSchema[EOr]    = schemaRecord[EOr]
  implicit val eVarSchema  : JsonSchema[EVar]   = schemaRecord[EVar]
  implicit val eListSchema : JsonSchema[EList]  = schemaRecord[EList]
  implicit val eTupleSchema: JsonSchema[ETuple] = schemaRecord[ETuple]
  
  implicit val hashSetParSchema      : JsonSchema[HashSet[Par]]     = schemaRecord[HashSet[Par]]
  implicit val listParSchema         : JsonSchema[List[Par]]        = schemaRecord[List[Par]]
  implicit val sortedParHashSetSchema: JsonSchema[SortedParHashSet] = schemaRecord[SortedParHashSet]
  implicit val coevalBitSetSchema    : JsonSchema[Coeval[BitSet]]   = schemaRecord[Coeval[BitSet]]
  implicit val optionVarSchema       : JsonSchema[Option[Var]]      = schemaRecord[Option[Var]]
  implicit val parSetSchema          : JsonSchema[ParSet]           = schemaRecord[ParSet]
  
  implicit val mapParParSchema    : JsonSchema[Map[Par, Par]]     = schemaRecord[Map[Par, Par]]
  implicit val listParParSchema   : JsonSchema[List[(Par, Par)]]  = schemaRecord[List[(Par, Par)]]
  implicit val hashMapParParSchema: JsonSchema[HashMap[Par, Par]] = schemaRecord[HashMap[Par, Par]]
  implicit val sortedParMapSchema : JsonSchema[SortedParMap]      = schemaRecord[SortedParMap]
  implicit val parMapSchema       : JsonSchema[ParMap]            = schemaRecord[ParMap]
  
  // implicit val eSetSchema   : JsonSchema[ESet]    = schemaRecord[ESet]
  // implicit val eMapSchema   : JsonSchema[EMap]    = schemaRecord[EMap]
  implicit val eMethodSchema: JsonSchema[EMethod] = schemaRecord[EMethod]

  implicit val eMatchesSchema       : JsonSchema[EMatches]        = schemaRecord[EMatches]
  implicit val ePercentPercentSchema: JsonSchema[EPercentPercent] = schemaRecord[EPercentPercent]
  implicit val ePlusPlusSchema      : JsonSchema[EPlusPlus]       = schemaRecord[EPlusPlus]
  implicit val eMinusMinusSchema    : JsonSchema[EMinusMinus]     = schemaRecord[EMinusMinus]
  implicit val eModSchema           : JsonSchema[EMod]            = schemaRecord[EMod]
  implicit val exprInstanceSchema   : JsonSchema[ExprInstance]    = schemaTagged[ExprInstance]
  implicit val exprSchema           : JsonSchema[Expr]            = schemaRecord[Expr]

  // Match
  implicit val matchCaseSchema: JsonSchema[MatchCase] = schemaRecord[MatchCase]
  implicit val matchSchema    : JsonSchema[Match]     = schemaRecord[Match]

  // GUnforgeable
  implicit val gPrivateSchema     : JsonSchema[GPrivate]      = schemaRecord[GPrivate]
  implicit val gDeployIdSchema    : JsonSchema[GDeployId]     = schemaRecord[GDeployId]
  implicit val gDeployerIdSchema  : JsonSchema[GDeployerId]   = schemaRecord[GDeployerId]
  implicit val gSysAuthTokenSchema: JsonSchema[GSysAuthToken] = schemaRecord[GSysAuthToken]
  implicit val unfInstanceSchema  : JsonSchema[UnfInstance]   = schemaTagged[UnfInstance]
  implicit val gUnforgeableSchema : JsonSchema[GUnforgeable]  = schemaRecord[GUnforgeable]

  // Bundle
  implicit val bundleSchema: JsonSchema[Bundle] = schemaRecord[Bundle]

  // Connective
  implicit val varRefSchema            : JsonSchema[VarRef]             = schemaRecord[VarRef]
  implicit val connectiveBodySchema    : JsonSchema[ConnectiveBody]     = schemaRecord[ConnectiveBody]
  implicit val connectiveInstanceSchema: JsonSchema[ConnectiveInstance] = schemaTagged[ConnectiveInstance]
  implicit val connectiveSchema        : JsonSchema[Connective]         = schemaRecord[Connective]
  
  // AlwaysEqual[BitSet]
  implicit val bitSetSchema           : JsonSchema[BitSet]              = schemaTagged[BitSet]
  implicit val alwaysEqualBitSetSchema: JsonSchema[AlwaysEqual[BitSet]] = schemaRecord[AlwaysEqual[BitSet]]

  // format: on

  // Json Schema encoding for ByteString as String base 16
  implicit lazy val byteStringSchema: JsonSchema[ByteString] =
    defaultStringJsonSchema.xmapPartial { str =>
      val bytesOpt = str.hexToByteString
      bytesOpt.map(Valid(_)).getOrElse(Invalid(s"Invalid hex format '$str'"))
    }(PrettyPrinter.buildStringNoLimit)
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
