/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

// Bb wraps Java ByteBuffer and adds some type genericity.
// Methods that translate a ByteBuffer to a Scala type
// resist type genericity due to ByteBuffer's lack of that.

package coop.rchain.storage

import java.nio.ByteBuffer
import java.nio.ByteBuffer.allocateDirect
import java.nio.charset.StandardCharsets.UTF_8

object Bb {

  def create[T](value: T): Option[ByteBuffer] = {
    if (value.isInstanceOf[Short]) {
      return Some(shortToBb(value.asInstanceOf[Short]))
    } else if (value.isInstanceOf[Int]) {
      return Some(intToBb(value.asInstanceOf[Int]))
    } else if (value.isInstanceOf[Long]) {
      return Some(longToBb(value.asInstanceOf[Long]))
    } else if (value.isInstanceOf[Float]) {
      return Some(floatToBb(value.asInstanceOf[Float]))
    } else if (value.isInstanceOf[Double]) {
      return Some(doubleToBb(value.asInstanceOf[Double]))
    } else if (value.isInstanceOf[String]) {
      return Some(strToBb(value.asInstanceOf[String]))
    }
    None
  }

  def shortToBb(value: Short): ByteBuffer = {
    val bb = ByteBuffer.allocateDirect(java.lang.Short.BYTES)
    bb.putInt(value).flip
    bb
  }

  def intToBb(value: Int): ByteBuffer = {
    // Notice assumed equivalence of Int and Integer
    val bb = ByteBuffer.allocateDirect(java.lang.Integer.BYTES)
    bb.putInt(value).flip
    bb
  }

  def longToBb(value: Long): ByteBuffer = {
    val bb = ByteBuffer.allocateDirect(java.lang.Long.BYTES)
    bb.putLong(value).flip
    bb
  }

  def floatToBb(value: Float): ByteBuffer = {
    val bb = ByteBuffer.allocateDirect(java.lang.Float.BYTES)
    bb.putFloat(value).flip
    bb
  }

  def doubleToBb(value: Double): ByteBuffer = {
    val bb = ByteBuffer.allocateDirect(java.lang.Double.BYTES)
    bb.putDouble(value).flip
    bb
  }

  def strToBb(str: String): ByteBuffer = {
    var bb = allocateDirect(str.getBytes.length)
    bb.put(str.getBytes(UTF_8)).flip
    bb
  }
  def bbToStr(buffer: ByteBuffer): String = {
    /*
    https://worldmodscode.wordpress.com/2012/12/14/the-java-bytebuffer-a-crash-course/
    suggests that the use of position() introduces a race condition a bug.
    Search for the third instance of "// NOT RECOMMENDED, don't do this"
     */
    val oldPosition = buffer.position
    val str = UTF_8.decode(buffer).toString()
    // reset buffer's position to its original so it is not altered:
    buffer.position(oldPosition)
    str
  }
}

/* Better code from Chris to be incorporated later:

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
    case x: Short => (java.lang.Short.BYTES, _.putShort(x))
    case x: Int => (java.lang.Integer.BYTES, _.putInt(x))
    case x: Long => (java.lang.Long.BYTES, _.putLong(x))
    case x: Float => (java.lang.Float.BYTES, _.putFloat(x))
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
}
 */
