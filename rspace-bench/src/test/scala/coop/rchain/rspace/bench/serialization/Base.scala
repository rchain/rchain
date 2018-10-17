package coop.rchain.rspace.bench.serialization

import java.nio.ByteBuffer

trait Serialize2ByteBuffer[A] {
  def encode(a: A): ByteBuffer
  def decode(bytes: ByteBuffer): A
}
