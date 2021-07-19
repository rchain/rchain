package com.revdefine.origin.revvaultexport.reporting

import cats.effect.Concurrent
import cats.implicits._
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueTypedStore, LmdbStoreManager}
import io.circe.Decoder.Result
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json, _}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

/**
  * This is used for decoding the json data stored in the transaction server database.
  * Directly reading from transaction store would be much faster than requesting the transaction server.
  */
object Transaction {

  type TransactionStore[F[_]] = KeyValueTypedStore[F, String, List[List[TransactionInfo]]]

  final case class deployInfoJ(
      deployer: String,
      term: String,
      timestamp: Long,
      sig: String,
      sigAlgorithm: String,
      phloPrice: Long,
      phloLimit: Long,
      validAfterBlockNumber: Long,
      cost: Long,
      errored: Boolean,
      systemDeployError: String
  )

  final case class TransactionInfo(
      fromAddr: String,
      toAddr: String,
      amount: Long,
      retUnforeable: String,
      deploy: deployInfoJ,
      success: Boolean,
      reason: String
  )

  implicit val deployInfoJDecoder: Decoder[deployInfoJ] = deriveDecoder[deployInfoJ]
  val decodeTransactionInfo: Decoder[List[List[TransactionInfo]]] =
    new Decoder[List[List[TransactionInfo]]] {
      final def apply(c: HCursor): Decoder.Result[List[List[TransactionInfo]]] =
        c.values
          .fold(
            DecodingFailure("the json needs a embedded array", List.empty)
              .asLeft[List[List[TransactionInfo]]]
          ) { embedListJson =>
            embedListJson
              .map(listJson => {
                listJson.asArray.fold(
                  DecodingFailure("inner object is array", List.empty)
                    .asLeft[List[TransactionInfo]]
                ) { jsonUnit =>
                  jsonUnit
                    .map(
                      j =>
                        for {
                          d             <- j.hcursor.downField("deploy").as[deployInfoJ]
                          fromAddr      <- j.hcursor.downField("fromAddr").as[String]
                          toAddr        <- j.hcursor.downField("toAddr").as[String]
                          amount        <- j.hcursor.downField("amount").as[Long]
                          retUnforeable <- j.hcursor.downField("retUnforeable").as[String]
                          success       <- j.hcursor.downField("success").as[Boolean]
                          reason        <- j.hcursor.downField("reason").as[String]
                        } yield TransactionInfo(
                          fromAddr = fromAddr,
                          toAddr = toAddr,
                          amount = amount,
                          retUnforeable = retUnforeable,
                          deploy = d,
                          success = success,
                          reason = reason
                        )
                    )
                    .toList
                    .traverse(identity)
                }
              })
              .toList
              .traverse(identity)
          }
    }

  def parseTransactionInfoJson(rawJson: String): Result[List[List[TransactionInfo]]] = {
    val parsed = parse(rawJson).getOrElse(Json.Null)
    val cursor = parsed.hcursor
    decodeTransactionInfo(cursor)
  }

  def store[F[_]: Concurrent: Log](
      dirPath: Path
  ): F[TransactionStore[F]] =
    LmdbStoreManager(dirPath, 1024L * 1024L * 1024L) >>= { manager =>
      manager
        .store("transactions")
        .map(_.toTypedStore(transactionKeyCodec, transactionValueCodec))
    }

  private val transactionKeyCodec = new Codec[String] {
    override def encode(value: String): Attempt[BitVector] =
      Attempt.successful(BitVector(value.getBytes("UTF8")))

    override def decode(bits: BitVector): Attempt[DecodeResult[String]] =
      Attempt.successful(
        DecodeResult(StandardCharsets.UTF_8.decode(bits.toByteBuffer).toString, BitVector.empty)
      )

    override def sizeBound: SizeBound = SizeBound.unknown
  }

  val transactionValueCodec: Codec[List[List[TransactionInfo]]] =
    new Codec[List[List[TransactionInfo]]] {
      override def encode(value: List[List[TransactionInfo]]): Attempt[BitVector] =
        Attempt.successful(BitVector(value.asJson.toString().getBytes(StandardCharsets.UTF_8)))

      override def decode(bits: BitVector): Attempt[DecodeResult[List[List[TransactionInfo]]]] = {
        val value = StandardCharsets.UTF_8.decode(bits.toByteBuffer).toString
        val target = if (value == "[]") {
          "[[]]"
        } else value
        val result = parseTransactionInfoJson(target)
        result match {
          case Right(value) => Attempt.successful(DecodeResult(value, BitVector.empty))
          case Left(f)      => Attempt.failure(Err.General(f.message, List.empty))
        }
      }

      override def sizeBound: SizeBound = SizeBound.unknown
    }
}
