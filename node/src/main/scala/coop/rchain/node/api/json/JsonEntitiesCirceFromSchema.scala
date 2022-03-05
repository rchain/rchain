package coop.rchain.node.api.json

import endpoints4s.algebra.circe.CirceCodec
import endpoints4s.{algebra, Invalid}
import org.http4s.circe
import org.http4s.circe.jsonEncoderOf

/**
  * Interpreter for circe JSON encoding from JSON schema
  */
trait JsonEntitiesCirceFromSchema
    extends algebra.JsonEntities
    with endpoints4s.http4s.server.EndpointsWithCustomErrors
    with endpoints4s.circe.JsonSchemas {

  def jsonRequest[A](implicit schema: JsonSchema[A]): RequestEntity[A] = {
    val decoder = JsonSchema.toCirceCodec(schema)
    JsonEntitiesCirce.decodeJsonRequest(this)(decoder)
  }

  def jsonResponse[A](implicit schema: JsonSchema[A]): ResponseEntity[A] = {
    val encoder = JsonSchema.toCirceCodec(schema)
    JsonEntitiesCirce.encodeJsonResponse(this)(encoder)
  }
}

private object JsonEntitiesCirce {

  def decodeJsonRequest[A](
      endpoints: endpoints4s.http4s.server.EndpointsWithCustomErrors
  )(codec: CirceCodec[A]): endpoints.RequestEntity[A] = {
    val cDecoder      = codec.decoder
    val entityDecoder = circe.jsonOf[endpoints.Effect, A](endpoints.Effect, cDecoder)
    req => {
      entityDecoder
        .decode(req, strict = false)
        .leftSemiflatMap { fail =>
          val invalid = Invalid(Seq(fail.message))
          endpoints.handleClientErrors(req, invalid)
        }(endpoints.Effect)
        .value
    }
  }

  def encodeJsonResponse[A](
      endpoints: endpoints4s.http4s.server.EndpointsWithCustomErrors
  )(codec: CirceCodec[A]): endpoints.ResponseEntity[A] = {
    val cEncoder = codec.encoder
    jsonEncoderOf[endpoints.Effect, A](cEncoder)
  }
}
