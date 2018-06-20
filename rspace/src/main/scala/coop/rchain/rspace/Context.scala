package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path

import coop.rchain.rspace.history.{ITrieStore, LMDBTrieStore}
import coop.rchain.rspace.internal.GNAT
import org.lmdbjava.{Env, EnvFlags, Txn}
import scodec.Codec

class Context[C, P, A, K] private (
    val env: Env[ByteBuffer],
    val path: Path,
    val trieStore: ITrieStore[Txn[ByteBuffer], Blake2b256Hash, GNAT[C, P, A, K]]
) {

  def close(): Unit = {
    trieStore.close()
    env.close()
  }
}

object Context {

  def create[C, P, A, K](path: Path,
                         mapSize: Long,
                         flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS))(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): Context[C, P, A, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val env: Env[ByteBuffer] =
      Env
        .create()
        .setMapSize(mapSize)
        .setMaxDbs(8)
        .setMaxReaders(126)
        .open(path.toFile, flags: _*)

    val trieStore = LMDBTrieStore.create[Blake2b256Hash, GNAT[C, P, A, K]](env)

    new Context(env, path, trieStore)
  }
}
