package coop.rchain.lmdb

import org.lmdbjava.ByteBufferProxy.PROXY_SAFE
import org.lmdbjava.{Env, EnvFlags}

import java.nio.ByteBuffer
import java.nio.file.Path

/**
  * TODO: This is temporary place for LMDB related things.
  *
  * This file is moved from RSpace which is now free from direct reference to LMDB
  * and here is already old implementation [[LMDBStore]].
  */
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
