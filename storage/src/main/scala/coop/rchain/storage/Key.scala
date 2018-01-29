package coop.rchain.storage

import java.nio.charset.StandardCharsets

/**
  * Represents the key in a key-value pair that will be stored in LMDB
  *
  * @param bytes the underlying byte-array, suitable for storage
  */
sealed abstract class Key(val bytes: Array[Byte])
final case class Hash(bs: Array[Byte])  extends Key(s"hash-".getBytes(StandardCharsets.UTF_8) ++ bs)
final case class Flat(value: String)    extends Key(s"flat-$value".getBytes(StandardCharsets.UTF_8))
final case class FlatSys(value: String) extends Key(s"fsys-$value".getBytes(StandardCharsets.UTF_8))
