package a

import java.util.UUID

class f { }


object RandomStringUUID {
    def main(args: Array[String]) {
      val uuid = UUID.randomUUID()
      val randomUUIDString = uuid.toString()

      println("Random UUID String = " + randomUUIDString)
      println("UUID version       = " + uuid.version())
      println("UUID variant       = " + uuid.variant())
    }
}

class uu {
  def getUUID () : String = {
    val so = UUID.randomUUID()
    println( " got here: " +  so)
    return so.toString()
  }
}

object DateTesting {

  import java.util.Date
  def beforeNow(implicit inDate: Date) = inDate before new Date()
  def afterNow(implicit inDate: Date) = inDate after new Date()

  def dateTesting () = {
    val sampleD: Long = 1491030000 //Sat Apr 01 00:00:00 PDT 2017
    val currentD: Long = System.currentTimeMillis/1000

    val sample01012017: Long = 1483257599 // Sat Dec 31 23:59:59 PST 2016

    println( sampleD + " " +new Date (sampleD * 1000))
    println( sample01012017 + " " +new Date (sample01012017 * 1000) + " " + (sampleD - sample01012017))
    println( currentD + " " +new Date (currentD * 1000))
    println( afterNow(new Date (sampleD * 1000)))
    val etheURL = "https://api.ethplorer.io/address/"
    val add = "0x6f0ce3df9e89a640a0dc5a0573182e623955dea5"
    val callURL = etheURL + add +"?apiKey=freekey"
    println (callURL)
    //val ethTol = scala.io.Source.fromURL(callURL).mkString.toDouble

    //println (ethTol)
  }

  def sMessage() = {

    import java.net._

    import r.net._

    val urlStr = "http://abc.dev.domain.com/0007AC/ads/800x480%2015sec%20h.264.mp4"
    val url = new URI(urlStr)
    val mker = new r.net.MURI (url)
    println(mker.getHost + " "+ mker.getScheme)

    val uuid = new UUIDOps {}
    println( uuid.getUUID(uuid.getUUID.toString))
    val samMessage = JustStrRequest (uuid.getUUID, mker, mker, uuid.getUUID(), "Helo world", None )


    println (samMessage + " \n->" + r.util.ConverterObjects.toString(samMessage))

    /*

    case class JustStrRequest(
  override val msgId  : UUID,
  //override val to     : URI,
  override val to     : Moniker,
  //override val from   : URI,
  override val from   : Moniker,
  override val flowId : UUID,
  override val body   : String,
  override val justification : Option[Response[AbstractJustifiedRequest[String,String],String]])
extends AbstractJustifiedRequest[String,String](msgId, to, from, flowId, body, justification)

     */

    /*

    new Moniker ("Scheme","UserInfo", "Authority", "Host", "Port", "Path", "Query", "Fragment", URI) {}
    trait Moniker extends Serializable {
def getScheme : String
def getUserInfo : String
def getAuthority : String
def getHost : String
def getPort : Int
def getPath : String
def getQuery : String
def getFragment : String
def uri : URI
}
     */


  }

  def main(args: Array[String]) {
    //var uobj = new uu()
    //println("Hello, world!" + uobj.getUUID())
    //sMessage()
    dateTesting()
  }
}