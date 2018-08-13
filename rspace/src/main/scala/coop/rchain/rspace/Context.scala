package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path

import coop.rchain.rspace.history.{ITrieStore, InMemoryTrieStore, LMDBTrieStore}
import coop.rchain.rspace.internal.GNAT
import org.lmdbjava.{Env, EnvFlags, Txn}
import scodec.Codec

trait Context[C, P, A, K] {
  def close(): Unit
}

class LMDBContext[C, P, A, K] private[rspace] (
    val env: Env[ByteBuffer],
    val path: Path,
    val trieStore: ITrieStore[Txn[ByteBuffer], Blake2b256Hash, GNAT[C, P, A, K]]
) extends Context[C, P, A, K] {

  def close(): Unit = {
    trieStore.close()
    env.close()
  }
}

class InMemoryContext[C, P, A, K] private[rspace] (
    val trieStore: ITrieStore[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]],
                              Blake2b256Hash,
                              GNAT[C, P, A, K]]
) extends Context[C, P, A, K] {
  def close(): Unit = {}
}

object Context {

  def env(path: Path,
          mapSize: Long,
          flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS)): Env[ByteBuffer] =
    Env
      .create()
      .setMapSize(mapSize)
      .setMaxDbs(8)
      .setMaxReaders(126)
      .open(path.toFile, flags: _*)

  def create[C, P, A, K](path: Path, mapSize: Long, noTls: Boolean)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): LMDBContext[C, P, A, K] = {
    val flags = if (noTls) List(EnvFlags.MDB_NOTLS) else List.empty
    create(path, mapSize, flags)
  }

  def create[C, P, A, K](path: Path,
                         mapSize: Long,
                         flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS))(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): LMDBContext[C, P, A, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val env = Context.env(path, mapSize, flags)

    val trieStore = LMDBTrieStore.create[Blake2b256Hash, GNAT[C, P, A, K]](env, path)

    new LMDBContext[C, P, A, K](env, path, trieStore)
  }

  def createInMemory[C, P, A, K](): InMemoryContext[C, P, A, K] = {
    val trieStore = InMemoryTrieStore.create[Blake2b256Hash, GNAT[C, P, A, K]]()
    new InMemoryContext(trieStore)
  }
}
