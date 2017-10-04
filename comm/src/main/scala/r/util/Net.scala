package r.util

import java.net.URI

import org.zeromq.ZMQ
import r.net.{JustStrRequest, UUIDOps}

object RServer {

  def start(inPort: String, inContext: Int) {
    //  Prepare our context and socket
    val port = inPort
    val context = ZMQ.context(inContext)
    val socket = context.socket(ZMQ.REP)
    print ("RServer: starting on port " +port)
    printf(" ZMQ Version string: %s, Version int: %d\n", ZMQ.getVersionString, ZMQ.getFullVersion)
    socket.bind ("tcp://*:"+ port)
    var counter = 0
    while (true) {
      counter += 1
      //  Wait for next request from client
      //  We will wait for a 0-terminated string (C string) from the client,
      //  so that this server also works with The Guide's C and C++ "Hello World" clients
      val request = socket.recv (0)
      val mess = new String(request)//,0,request.length-1) //.asInstanceOf[MC]
      println ("mess -> :"+ mess)
      //  In order to display the 0-terminated string as a String,
      //  we omit the last byte from request  // Avoid runtime pickler

      // Import pickle ops  // Avoid runtime pickler

      // Import pickle ops

      //val upMessage = mess.unpickle [JustStrRequest]


      val mO = ConverterObjects.toObject(mess)//.asInstanceOf[r.net.Request]
      //val tO = ConverterObjects.toObject(mess).asInstanceOf[coop.rchain.trie.Trie] //mess.asInstanceOf[MC]
      print(s"Received request: ${counter} ["
        //  + new String(request,0,request.length-1)  //  Creates a String from request, minus the last byte
        //  + "]")
        + mess.toString + "]")
      //print( "`"+mO.s1 + "` `" +mO.s2+"`" + " `"+mO.i1 + "` `" +mO.i2+"`")
      //print( "`"+ coop.rchain.trie.Trie.get ("mom", "123123"))
      //  Do some 'work'
      try {
        Thread.sleep (1000)
      } catch  {
        case e: InterruptedException => e.printStackTrace()
      }

      //  Send reply back to client
      //  We will send a 0-terminated string (C string) back to the client,
      //  so that this server also works with The Guide's C and C++ "Hello World" clients
      val reply = "World ".getBytes
      reply(reply.length-1) = 0 //Sets the last byte of the reply to 0
      socket.send(reply, 0)
    }
  }
  def main(args : Array[String]): Unit = {
    start ("5555", 1)
  }
}

object RClient{

  def start ( inServer: String, inPort: String, inContext: Int, inAny: Serializable): Unit = {

    //  Prepare our context and socket
    val context = ZMQ.context(inContext)
    val socket = context.socket(ZMQ.REQ)
    //val m = new MC("hello 1","hello 2", 567, 23432)
    //println(m.toString+ " " + m.getClass + m.asInstanceOf[MC].s1)
    //val m = new JustStrRequest(UUID.randomUUID(),"toServer", "fromClient",UUID.randomUUID(), "This Body", None )
    println("Connecting to hello world server…")
    socket.connect ("tcp://"+inServer+":"+inPort)
    val request = r.util.ConverterObjects.toString(inAny)
    socket.send(request, 0)
    val reply = socket.recv(0)
    println("Received reply : [" + new String(reply,0,reply.length-1) + "]")
  }

  def main(args : Array[String]): Unit = {
    //val m = new a.MC("hello 1","hello 2", 567, 23432)
    val urlStr = "http://abc.dev.domain.com/0007AC/ads/800x480%2015sec%20h.264.mp4"
    val url = new URI(urlStr)
    val mker = new r.net.MURI (url)
    println(mker.getHost + " "+ mker.getScheme)

    val uuid = new UUIDOps {}
    println( uuid.getUUID(uuid.getUUID.toString))
    val samMessage = JustStrRequest (uuid.getUUID, mker, mker, uuid.getUUID(), "Hello world", None )  // Avoid runtime pickler

    // Import pickle ops
    // Alternatively import pickle function
    // import scala.pickling.functions._

    // Import picklers for specific types
    //import scala.pickling.Defaults.{ stringPickler, intPickler, refPicklerUnpickler, nullPickler }


    //val words = Source.fromFile("/Users/n/Downloads/wordsEn.txt").getLines.toArray
    //val t1 = coop.rchain.trie.Trie.root("mom://")
    //words.map(x => Trie.put ("words://", x, x))
    //println("samMessage: " +r.util.ConverterObjects.toString(samMessage))
    //println("samMessage: " +pMessage)
    //start ("localhost", "5555", 1, pMessage)
  }
}
