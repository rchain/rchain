/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

// Bb wraps Java ByteBuffer and adds type genericity.

package coop.rchain.storage

import java.nio.ByteBuffer
import java.nio.ByteBuffer.allocateDirect
import java.nio.charset.StandardCharsets.UTF_8

object Bb {
  trait Bbable[T]
  object Bbable {
    implicit object ShortEv extends Bbable[Short]
    implicit object IntEv extends Bbable[Int]
    implicit object LongEv extends Bbable[Long]
    implicit object FloatEv extends Bbable[Float]
    implicit object DoubleEv extends Bbable[Double]
    implicit object StringEv extends Bbable[String]
  }

  def sizeof[T: Bbable](data: T): (Int, (ByteBuffer) => Any) = data match {
    case x: Short  => (java.lang.Short.BYTES, _.putShort(x))
    case x: Int    => (java.lang.Integer.BYTES, _.putInt(x))
    case x: Long   => (java.lang.Long.BYTES, _.putLong(x))
    case x: Float  => (java.lang.Float.BYTES, _.putFloat(x))
    case x: Double => (java.lang.Double.BYTES, _.putDouble(x))
    case x: String => { val bs = x.getBytes(UTF_8); (bs.length, _.put(bs)) }
  }

  def create[T: Bbable](data: T): Option[ByteBuffer] = {
    val (sz, f) = sizeof(data)
    val bb = ByteBuffer.allocateDirect(sz)
    f(bb)
    bb.flip
    Some(bb)
  }

  def bbToStr(bb: ByteBuffer): String = {
    // https://worldmodscode.wordpress.com/2012/12/14/the-java-bytebuffer-a-crash-course/
    val bytes: Array[Byte] = new Array[Byte](bb.remaining)
    bb.duplicate.get(bytes)
    new String(bytes)
  }
}
