package coop.rchain.rspace

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.history.{Branch, ITrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.spaces.{CoarseGrainedRSpace, FineGrainedRSpace}
import scodec.Codec

object RSpace {

  def create[F[_], C, P, E, A, R, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): F[ISpace[F, C, P, E, A, R, K]] =
    context match {
      case ctx: LMDBContext[C, P, A, K] =>
        create(LMDBStore.create[C, P, A, K](ctx, branch), branch)

      case ctx: InMemoryContext[C, P, A, K] =>
        createInMemory(ctx.trieStore, branch)

      case ctx: MixedContext[C, P, A, K] =>
        create(InMemoryStore.create(ctx.trieStore, branch), branch)
    }

  def createInMemory[F[_], C, P, E, A, R, K](
      trieStore: ITrieStore[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], Blake2b256Hash, GNAT[
        C,
        P,
        A,
        K
      ]],
      branch: Branch
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): F[ISpace[F, C, P, E, A, R, K]] = {

    val mainStore = InMemoryStore
      .create[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], C, P, A, K](
        trieStore,
        branch
      )
    create(mainStore, branch)
  }

  def create[F[_], C, P, E, A, R, K](store: IStore[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): F[ISpace[F, C, P, E, A, R, K]] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val space: ISpace[F, C, P, E, A, R, K] =
      new CoarseGrainedRSpace[F, C, P, E, A, R, K](store, branch)

    /*
     * history.initialize returns true if the history trie contains no root (i.e. is empty).
     *
     * In this case, we create a checkpoint for the empty store so that we can reset
     * to the empty store state with the clear method.
     */
    if (history.initialize(store.trieStore, branch)) {
      space.createCheckpoint().map(_ => space)
    } else {
      space.pure[F]
    }
  }

  def createFineGrained[F[_], C, P, E, A, R, K](store: IStore[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): F[ISpace[F, C, P, E, A, R, K]] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val space: ISpace[F, C, P, E, A, R, K] =
      new FineGrainedRSpace[F, C, P, E, A, R, K](store, branch)

    /*
     * history.initialize returns true if the history trie contains no root (i.e. is empty).
     *
     * In this case, we create a checkpoint for the empty store so that we can reset
     * to the empty store state with the clear method.
     */
    if (history.initialize(store.trieStore, branch)) {
      space.createCheckpoint().map(_ => space)
    } else {
      space.pure[F]
    }
  }
}
