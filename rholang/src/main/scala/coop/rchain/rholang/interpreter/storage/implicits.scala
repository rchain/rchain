package coop.rchain.rholang.interpreter.storage

import cats.implicits._
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.rholang.interpreter.SpatialMatcher._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rspace.{Serialize, Match => StorageMatch}
import scodec.bits.ByteVector

//noinspection ConvertExpressionToSAM
object implicits {

  /* Match instance */

  private def toChannels(fm: FreeMap, max: Int): Seq[Channel] =
    (0 until max).map { (i: Int) =>
      fm.get(i) match {
        case Some(par) => Channel(Quote(par))
        case None      => Channel(Quote(Par.defaultInstance))
      }
    }

  implicit val matchListQuote: StorageMatch[BindPattern, Seq[Channel]] =
    new StorageMatch[BindPattern, Seq[Channel]] {

      def get(pattern: BindPattern, data: Seq[Channel]): Option[Seq[Channel]] =
        foldMatch(data, pattern.patterns, pattern.remainder)
          .run(emptyMap)
          .map {
            case (freeMap: FreeMap, caughtRem: Seq[Channel]) =>
              val remainderMap = pattern.remainder match {
                case Some(Var(FreeVar(level))) =>
                  val flatRem: Seq[Par] = caughtRem.flatMap(
                    chan =>
                      chan match {
                        case Channel(Quote(p)) => Some(p)
                        case _                 => None
                    }
                  )
                  freeMap + (level -> VectorPar().addExprs(EList(flatRem.toVector)))
                case _ => freeMap
              }
              toChannels(remainderMap, pattern.freeCount)
          }
    }

  /* Serialize instances */

  implicit val serializeBindPattern: Serialize[BindPattern] =
    mkProtobufInstance(BindPattern)

  implicit val serializeChannel: Serialize[Channel] =
    mkProtobufInstance(Channel)

  implicit val serializeChannels: Serialize[Seq[Channel]] =
    new Serialize[Seq[Channel]] {

      override def encode(a: Seq[Channel]): ByteVector =
        ByteVector.view(ListChannel.toByteArray(ListChannel(a)))

      override def decode(bytes: ByteVector): Either[Throwable, Seq[Channel]] =
        Either.catchNonFatal(ListChannel.parseFrom(bytes.toArray).channels.toList)
    }

  implicit val serializeTaggedContinuation: Serialize[TaggedContinuation] =
    mkProtobufInstance(TaggedContinuation)
}
