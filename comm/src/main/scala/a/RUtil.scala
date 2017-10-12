package a

import java.io._

import scala.collection.mutable.ArrayBuffer

class Util {
  //def fileToString = FileUtils.readFileToString(file, StandardCharsets.UTF_8)
  def fileToString(file: File, encoding: String) = {
    val iStream = new FileInputStream(file)
    val oStream = new ByteArrayOutputStream
    try {
      var reading = true
      while ( reading ) {
        iStream.read() match {
          case -1 => reading = false
          case c => oStream.write(c)
        }
      }
      oStream.flush()
    }
    finally {
      iStream.close()
    }
    new String(oStream.toByteArray(), encoding)
  }

  def toBytes(xs: Int*) = xs.map(_.toByte).toArray
  //xs.iterator.map(_.toByte).toArray

  /*
  def ObjectToByteArray(obj: Any): Array[Byte] = {
    if(obj == null) return null
    BinaryFormatter bf = new BinaryFormatter()
    MemoryStream ms = new MemoryStream()
    bf.Serialize(ms, obj)
    return ms.ToArray()
  }

  // Convert a byte array to an Object
  def ByteArrayToObject(arrBytes: Array[T]) {
    MemoryStream memStream = new MemoryStream()
    BinaryFormatter binForm = new BinaryFormatter()
    memStream.Write(arrBytes, 0, arrBytes.Length)
    memStream.Seek(0, SeekOrigin.Begin)
    Object obj = (Object) binForm.Deserialize(memStream)
    return obj
  }
  */
}

object StringConverter {
  val INTBYTES:Int = 4 // int is 4 bytes
  val SIZEBYTE:Short = 8

  def toArrayBuf(x:Int): ArrayBuffer[Byte] = {
    val buf = new ArrayBuffer[Byte](INTBYTES)
    for(i <- 0 until INTBYTES) {
      buf += ((x >>> (INTBYTES - i - 1 << 3)) & 0xFF).toByte
    }
    buf
  }

  def toBinaryString(x: Byte): String = {
    val buf = new StringBuilder(SIZEBYTE)
    for(i <- 0 until SIZEBYTE) {
      buf.append((x >>> (SIZEBYTE - i - 1)) & 0x01)
    }
    buf.toString()
  }
  //pimp Byte
  implicit def fooBar(byte: Byte) = new {def toBinaryString = StringConverter.toBinaryString(byte)}

}

object HexBytesUtil {

  def hex2bytes(hex: String): Array[Byte] = {
    if(hex.contains(" ")){
      hex.split(" ").map(Integer.parseInt(_, 16).toByte)
    } else if(hex.contains("-")){
      hex.split("-").map(Integer.parseInt(_, 16).toByte)
    } else {
      hex.sliding(2,2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
  }

  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None =>  bytes.map("%02x".format(_)).mkString
      case _ =>  bytes.map("%02x".format(_)).mkString(sep.get)
    }
    // bytes.foreach(println)
  }

  def example {
    val data = "48 65 6C 6C 6F 20 57 6F 72 6C 64 21 21"
    val bytes = hex2bytes(data)
    println(bytes2hex(bytes, Option(" ")))

    val data2 = "48-65-6C-6C-6F-20-57-6F-72-6C-64-21-21"
    val bytes2 = hex2bytes(data2)
    println(bytes2hex(bytes2, Option("-")))

    val data3 = "48656C6C6F20576F726C642121"
    val bytes3 = hex2bytes(data3)
    println(bytes2hex(bytes3))
  }

}

class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }

object L extends CC[List[Any]]

object M extends CC[Map[String, Any]]
object S extends CC[String]
object D extends CC[Double]
object B extends CC[Boolean]

object SampleJasonParser {
  import scala.util.parsing.json._

  val jsonString =
    """
  |{
  | "languages": [{
  |     "name": "English",
  |     "is_active": true,
  |     "completeness": 2.5
  | }, {
  |     "name": "Latin",
  |     "is_active": false,
  |     "completeness": 0.9
  | }]
  |}
""".stripMargin

  val result = for { Some(M(map)) <- List(JSON.parseFull(jsonString))
    a.L(languages) = map("languages")
    a.M(language) <- languages
    a.S(name) = language("name")
    a.B(active) = language("is_active")
    a.D(completeness) = language ("completeness")
  } yield {(name, active, completeness)
  }
  println (result)
  assert( result == List(("English",true,2.5), ("Latin",false, 0.9)))

  def main (args: Array[String]): Unit = {
    val par = SampleJasonParser
  }
}