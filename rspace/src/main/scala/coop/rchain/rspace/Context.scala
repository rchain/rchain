package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path

import cats.effect.Sync
import coop.rchain.rspace.history.{Branch, ITrieStore, InMemoryTrieStore, LMDBTrieStore}
import coop.rchain.rspace.internal.GNAT
import org.lmdbjava.{Env, EnvFlags, Txn}
import scodec.Codec

trait Context[F[_], C, P, A, K] {
  def close(): Unit
  def createStore(branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): IStore[F, C, P, A, K]
}

private[rspace] class LMDBContext[F[_], C, P, A, K] private[rspace] (
    val env: Env[ByteBuffer],
    val path: Path,
    val trieStore: ITrieStore[Txn[ByteBuffer], Blake2b256Hash, GNAT[C, P, A, K]]
) extends Context[F, C, P, A, K] {
  override def createStore(branch: Branch)(
      implicit sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): IStore[F, C, P, A, K] =
    LMDBStore.create[F, C, P, A, K](this, branch)

  def close(): Unit = {
    trieStore.close()
    env.close()
  }
}

private[rspace] class InMemoryContext[F[_], C, P, A, K] private[rspace] (
    val trieStore: ITrieStore[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], Blake2b256Hash, GNAT[
      C,
      P,
      A,
      K
    ]]
) extends Context[F, C, P, A, K] {
  override def createStore(branch: Branch)(
      implicit sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): IStore[F, C, P, A, K] =
    InMemoryStore.create(trieStore, branch)

  def close(): Unit = {}
}

private[rspace] class MixedContext[F[_], C, P, A, K] private[rspace] (
    val env: Env[ByteBuffer],
    val trieStore: ITrieStore[Txn[ByteBuffer], Blake2b256Hash, GNAT[C, P, A, K]]
) extends Context[F, C, P, A, K] {

  override def createStore(branch: Branch)(
      implicit sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): IStore[F, C, P, A, K] =
    LockFreeInMemoryStore.create(trieStore, branch)

  def close(): Unit = {
    trieStore.close()
    env.close()
  }
}

object Context {

  def env(
      path: Path,
      mapSize: Long,
      flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS)
  ): Env[ByteBuffer] =
    Env
      .create()
      .setMapSize(mapSize)
      .setMaxDbs(8)
      .setMaxReaders(2048)
      .open(path.toFile, flags: _*)

  def create[F[_], C, P, A, K](path: Path, mapSize: Long, noTls: Boolean)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): LMDBContext[F, C, P, A, K] = {
    val flags = if (noTls) List(EnvFlags.MDB_NOTLS) else List.empty
    create(path, mapSize, flags)
  }

  def create[F[_], C, P, A, K](
      path: Path,
      mapSize: Long,
      flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS)
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): LMDBContext[F, C, P, A, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val env = Context.env(path, mapSize, flags)

    val trieStore = LMDBTrieStore.create[F, Blake2b256Hash, GNAT[C, P, A, K]](env, path)

    new LMDBContext[F, C, P, A, K](env, path, trieStore)
  }

  def createInMemory[F[_], C, P, A, K](): InMemoryContext[F, C, P, A, K] = {
    val trieStore = InMemoryTrieStore.create[Blake2b256Hash, GNAT[C, P, A, K]]()
    new InMemoryContext(trieStore)
  }

  def createMixed[F[_], C, P, A, K](
      path: Path,
      mapSize: Long,
      flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS)
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      syncF: Sync[F]
  ): MixedContext[F, C, P, A, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val env = Context.env(path, mapSize, flags)

    val trieStore = LMDBTrieStore.create[F, Blake2b256Hash, GNAT[C, P, A, K]](env, path)

    new MixedContext[F, C, P, A, K](env, trieStore)
  }

}
