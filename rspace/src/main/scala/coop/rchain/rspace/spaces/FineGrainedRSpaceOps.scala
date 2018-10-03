package coop.rchain.rspace.spaces

import scala.collection.immutable.Seq

import cats.effect.Sync
import coop.rchain.rspace._
import coop.rchain.rspace.concurrent.{DefaultTwoStepLock, TwoStepLock}
import coop.rchain.rspace.history.Branch

abstract class FineGrainedRSpaceOps[F[_], C, P, E, A, R, K](
    store: IStore[C, P, A, K],
    branch: Branch
)(
    implicit
    serializeC: Serialize[C],
    serializeP: Serialize[P],
    serializeA: Serialize[A],
    serializeK: Serialize[K],
    syncF: Sync[F]
) extends RSpaceOps[F, C, P, E, A, R, K](store, branch) {

  implicit val codecC = serializeC.toCodec

  private val lock: TwoStepLock[Blake2b256Hash] = new DefaultTwoStepLock()

  protected[this] def consumeLock(
      channels: Seq[C]
  )(thunk: => Either[E, Option[(K, Seq[R])]]): Either[E, Option[(K, Seq[R])]] = {
    val hashes = channels.map(ch => StableHashProvider.hash(ch))
    lock.acquire(hashes)(() => hashes)(thunk)
  }

  protected[this] def produceLock(
      channel: C
  )(thunk: => Either[E, Option[(K, Seq[R])]]): Either[E, Option[(K, Seq[R])]] =
    lock.acquire(Seq(StableHashProvider.hash(channel)))(
      () =>
        store.withTxn(store.createTxnRead()) { txn =>
          val groupedChannels: Seq[Seq[C]] = store.getJoin(txn, channel)
          groupedChannels.flatten.map(StableHashProvider.hash(_))
        }
    )(thunk)

}
