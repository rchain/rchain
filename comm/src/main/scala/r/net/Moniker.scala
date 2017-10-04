package r.net

import java.net.URI
import java.net.URL

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
case class MURI( override val uri : URI ) extends Moniker {
  override def getScheme : String = uri.getScheme
  override def getUserInfo : String = uri.getUserInfo
  override def getAuthority : String = uri.getAuthority
  override def getHost : String = uri.getHost
  override def getPort : Int = uri.getPort
  override def getPath : String = uri.getPath
  override def getQuery : String = uri.getQuery
  override def getFragment : String = uri.getFragment
}
case class MURN( override val uri : URI ) extends Moniker {
  override def getScheme : String = uri.getScheme
  override def getUserInfo : String = uri.getUserInfo
  override def getAuthority : String = uri.getAuthority
  override def getHost : String = uri.getHost
  override def getPort : Int = uri.getPort
  override def getPath : String = uri.getPath
  override def getQuery : String = uri.getQuery
  override def getFragment : String = uri.getFragment
}
case class MURL( url : URL ) extends Moniker {
  override def getScheme : String = url.getProtocol
  override def getUserInfo : String = url.getUserInfo
  override def getAuthority : String = url.getAuthority
  override def getHost : String = url.getHost
  override def getPort : Int = url.getPort
  override def getPath : String = url.getPath
  override def getQuery : String = url.getQuery
  override def getFragment : String = ""
  override lazy val uri : URI = new URI(getScheme, getUserInfo, getHost, getPort, getPath, getQuery, getFragment )
}

class URM(
    val scheme : String,
    val userInfo : Option[String],
    val authority : Option[String],
    val host : String,
    val port : Option[Int],
    val path : String,
    val query : Option[String],
    val fragment : Option[String])
  extends Moniker {

  def this(scheme: String, host: String, path: String, fragment: Option[String]) = this (scheme, None, None, host, None, path, None, fragment)

  override lazy val uri =
  {
    val qry = query.getOrElse( "" )
    val frg = fragment.getOrElse( "" )

    userInfo match {
      case Some( usr ) => port match {
          case Some( p ) => new URI(scheme, usr, host, p, path, qry, frg )
          case _ =>  throw new Exception( "invalid arguments: userInfo without port")
        }
      case _ => authority match {
          case Some( auth ) => new URI(scheme, auth, path, qry, frg )
          case _ => new URI(scheme, host, path, frg )
        }
    }
  }

  def significantBit( path : String ) = ""
  override def equals( theOther : Any ) : Boolean = {
    (
      theOther match {
        case that : URM => {
          (
            scheme.equals( that.scheme )
              && host.equals( that.host )
              && significantBit( path ).equals( significantBit( that.path ))
              && fragment.equals( that.fragment )
              && port.equals( that.port )
            )
        }
        case that : Moniker => {
          (
            scheme.equals( that.getScheme )
              && host.equals( that.getHost )
              && significantBit( path ).equals( significantBit( that.getPath ) )
              && fragment.equals( that.getFragment )
              && port.equals( that.getPort )
            )
        }
        case _ => false
      }
    )
  }
  override def hashCode( ) : Int = {
    (
      ( 37 * scheme.hashCode )
        + ( 37 * host.hashCode )
        + ( 37 * significantBit( path ).hashCode )
        + ( 37 * fragment.hashCode )
        + ( 37 * port.hashCode )
      )
  }
  override def getScheme : String = uri.getScheme
  override def getUserInfo : String = uri.getUserInfo
  override def getAuthority : String = uri.getAuthority
  override def getHost : String = uri.getHost
  override def getPort : Int = uri.getPort
  override def getPath : String = uri.getPath
  override def getQuery : String = uri.getQuery
  override def getFragment : String = uri.getFragment
  override def toString : String = { "URM(" + uri.toString + ")" }
}


object identityConversions {
  def defaultScheme : String = "specialk"
  def defaultPath : String = "/connection"
  def defaultFragment : String = ""

  implicit def toMoniker( url : URL ) : Moniker = MURL( url )
  implicit def toMoniker( uri : URI ) : Moniker = MURI( uri )
  implicit def toMoniker( s : String ) : Moniker = new URI( defaultScheme, s, defaultPath, defaultFragment )
  implicit def toMoniker( s : Symbol ) : URI = new URI( defaultScheme, s.toString.replace( "'", "" ), defaultPath, defaultFragment)

  implicit def toURI( mnkr : Moniker ) : URI = {
    mnkr match {
      case muri : MURI => muri.uri
      case murn : MURN => murn.uri
      case urm : URM => urm.uri
      case _ => throw new Exception( "conversion not defined" )
    }
  }

  implicit def toURL( mnkr : Moniker ) : URL = {
    mnkr match {
      case murl : MURL => murl.url
      case _ => throw new Exception( "conversion not defined" )
    }
  }

  implicit def toURM( mnkr : MURI ) : URM = {
    new URM(
      mnkr.getScheme,
      Some( mnkr.getUserInfo ),
      Some( mnkr.getAuthority ),
      mnkr.getHost,
      Some( mnkr.getPort ),
      mnkr.getPath,
      Some( mnkr.getQuery ),
      Some( mnkr.getFragment )
    )
  }
}
