/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.Storage

import java.nio.ByteBuffer
import java.nio.ByteBuffer.allocateDirect
import java.nio.charset.StandardCharsets.UTF_8


// Java ByteBuffer
object Bb {

  def toBb[T](value:T): Option[ByteBuffer] = {
    if (value.isInstanceOf[Short]) {
      return Some(shortToBb(value.asInstanceOf[Short]))
    }
    else if (value.isInstanceOf[Int]) {
      return Some(intToBb(value.asInstanceOf[Int]))
    }
    else if (value.isInstanceOf[Long]) {
      return Some(longToBb(value.asInstanceOf[Long]))
    }
    else if (value.isInstanceOf[Float]) {
      return Some(floatToBb(value.asInstanceOf[Float]))
    }
    else if (value.isInstanceOf[Double]) {
      return Some(doubleToBb(value.asInstanceOf[Double]))
    }
    else if (value.isInstanceOf[String]) {
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
    var bb = allocateDirect(str.length)
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
