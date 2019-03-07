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
