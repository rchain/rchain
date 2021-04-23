package coop.rchain.rholang.interpreter

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Span
import coop.rchain.models.{ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.RhoRuntime.RhoTuplespace
import coop.rchain.rspace.util.unpackCont

/**
  * This is a tool for unapplying the messages sent to the system contracts.
  *
  * The unapply returns (Producer, Seq[Par]).
  *
  * The Producer is the function with the signature (Seq[Par], Par) => F[Unit] which can be used to send a message
  * through a channel. The first argument with type Seq[Par] is the content of the message and the second argument is
  * the channel.
  *
  * Note that the random generator and the sequence number extracted from the incoming message are required for sending
  * messages back to the caller so they are given as the first argument list to the produce function.
  *
  * The Seq[Par] returned by unapply contains the message content and can be further unapplied as needed to match the
  * required signature.
  *
  * @param space the rspace instance
  * @param dispatcher the dispatcher
  */
class ContractCall[F[_]: Concurrent: Span](
    space: RhoTuplespace[F],
    dispatcher: Dispatch[F, ListParWithRandom, TaggedContinuation]
) {
  type Producer = (Seq[Par], Par) => F[Unit]

  // TODO: pass _cost[F] as an implicit parameter
  private def produce(
      rand: Blake2b512Random
  )(values: Seq[Par], ch: Par): F[Unit] =
    for {
      produceResult <- space.produce(
                        ch,
                        ListParWithRandom(values, rand),
                        persist = false
                      )
      _ <- produceResult.fold(Sync[F].unit) {
            case (cont, channels) =>
              dispatcher.dispatch(
                unpackCont(cont),
                channels.map(_.matchedDatum)
              )
          }
    } yield ()

  def unapply(contractArgs: Seq[ListParWithRandom]): Option[(Producer, Seq[Par])] =
    contractArgs match {
      case Seq(
          ListParWithRandom(
            args,
            rand
          )
          ) =>
        Some((produce(rand), args))
      case _ => None
    }
}
