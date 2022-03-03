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
import coop.rchain.models.Par
import coop.rchain.models.syntax._
import coop.rchain.node.api.WebApi._
import coop.rchain.node.web.TransactionInfo
import endpoints4s.{Invalid, Valid}

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
