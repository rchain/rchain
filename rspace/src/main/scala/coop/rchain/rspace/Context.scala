package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path

import org.lmdbjava.{Env, EnvFlags, Txn}
import org.lmdbjava.ByteBufferProxy.PROXY_SAFE

object Context {

  def env(
      path: Path,
      mapSize: Long,
      flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS, EnvFlags.MDB_NORDAHEAD)
  ): Env[ByteBuffer] =
    Env
      .create(PROXY_SAFE)
      .setMapSize(mapSize)
      .setMaxDbs(8)
      .setMaxReaders(2048)
      .open(path.toFile, flags: _*)
}
