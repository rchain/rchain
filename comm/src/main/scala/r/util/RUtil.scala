package r.util

import java.io._
import java.util.Base64

/** General purpose Utilities
  *
  */
class RUtil { }
object ConverterObjects {
  /** Read the object from Base64 string. */
  def toObject(inString: String): java.lang.Object = {
    val data: Array[Byte] = Base64.getDecoder.decode(inString)
    val oInputStream: ObjectInputStream =
      new ObjectInputStream(new ByteArrayInputStream(data))
    val outObject = oInputStream.readObject
    oInputStream.close()
    outObject
  }

  /** Write the object to a Base64 string. */
  def toString(inObject: Serializable): String = {
    val baos: ByteArrayOutputStream = new ByteArrayOutputStream
    val oos: ObjectOutputStream = new ObjectOutputStream(baos)
    oos.writeObject(inObject)
    oos.close()
    new String(Base64.getEncoder.encode(baos.toByteArray))
  }

  /*
  def appendZero (inByte: Array[Byte]) : Array[Byte] = {
    val outAB = inByte.
    None
  }
*/
}