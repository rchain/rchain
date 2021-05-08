package coop.rchain.rspace.channelStore.syntax

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.rspace.channelStore.{ChannelStore, ContinuationHash, DataJoinHash}
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.shared.syntax._

trait ChannelStoreSyntax {
  implicit final def syntaxChannelStore[F[_], C](
      channelStore: ChannelStore[F, C]
  ): ChannelStoreOps[F, C] =
    new ChannelStoreOps[F, C](channelStore)
}

final case class ProduceMapping(historyHash: Blake2b256Hash, eventLogHash: Blake2b256Hash)
final case class ConsumeMapping(historyHash: Blake2b256Hash, eventLogHashes: Seq[Blake2b256Hash])

// TODO remove
class ChannelStoreOps[F[_], C](
    private val channelStore: ChannelStore[F, C]
) {
  def getProduceMappings(
      produces: Seq[Produce]
  )(implicit c: Concurrent[F]): F[Vector[ProduceMapping]] =
    fs2.Stream
      .emits(
        produces
          .map(_.channelsHash)
          .toList
          .map(
            h =>
              fs2.Stream
                .eval(
                  channelStore.getChannelHash(h).flatMap {
                    case Some(DataJoinHash(dataHash, _)) => ProduceMapping(dataHash, h).some.pure[F]
                    case _                               => none[ProduceMapping].pure[F]
                  }
                )
                .filter(_.isDefined)
                .map(_.get)
          )
      )
      .parJoinProcBounded
      .compile
      .toVector

  def getConsumeMappings(
      consumes: Seq[Consume]
  )(implicit c: Concurrent[F]): F[Vector[ConsumeMapping]] =
    fs2.Stream
      .emits(
        consumes
          .map(_.channelsHashes)
          .toList
          .map(
            channelHashes => {
              val contKey = channelStore.continuationKey(channelHashes)
              fs2.Stream
                .eval(
                  for {
                    r <- channelStore.getChannelHash(contKey).flatMap {
                          case Some(ContinuationHash(consumeHash)) =>
                            ConsumeMapping(consumeHash, channelHashes).some.pure[F]
                          case _ => none[ConsumeMapping].pure[F]
                        }

                  } yield r
                )
                .filter(_.isDefined)
                .map(_.get)
            }
          )
      )
      .parJoinProcBounded
      .compile
      .toVector
}
