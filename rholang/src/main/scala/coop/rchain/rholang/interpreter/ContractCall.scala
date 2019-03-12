package coop.rchain.rholang.interpreter
import cats.effect.Sync
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.{ListParWithRandom, ListParWithRandomAndPhlos, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rspace.util.unpackCont
import cats.implicits._

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
class ContractCall[F[_]: Sync](
    space: RhoISpace[F],
    dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
) {
  type Producer = (Seq[Par], Par) => F[Unit]

  private val UNLIMITED_MATCH_PHLO = matchListPar(Cost(Integer.MAX_VALUE))

  private def produce(
      rand: Blake2b512Random,
      sequenceNumber: Int
  )(values: Seq[Par], ch: Par): F[Unit] =
    for {
      produceResult <- space.produce(
                        ch,
                        ListParWithRandom(values, rand),
                        persist = false,
                        sequenceNumber
                      )(UNLIMITED_MATCH_PHLO)
      _ <- produceResult.fold(
            _ => Sync[F].raiseError(OutOfPhlogistonsError),
            _.fold(Sync[F].unit) {
              case (cont, channels) =>
                dispatcher.dispatch(unpackCont(cont), channels.map(_.value), cont.sequenceNumber)
            }
          )
    } yield ()

  def unapply(contractArgs: (Seq[ListParWithRandomAndPhlos], Int)): Option[(Producer, Seq[Par])] =
    contractArgs match {
      case (
          Seq(
            ListParWithRandomAndPhlos(
              args,
              rand,
              _
            )
          ),
          sequenceNumber
          ) =>
        Some((produce(rand, sequenceNumber), args))
      case _ => None
    }
}
