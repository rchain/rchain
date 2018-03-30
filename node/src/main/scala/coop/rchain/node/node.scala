package coop.rchain.node

import coop.rchain.p2p, p2p.NetworkAddress, p2p.Network.KeysStore
import coop.rchain.comm._, CommError._
import com.typesafe.scalalogging.Logger
import java.net.{InetAddress, NetworkInterface}
import java.util.UUID
import java.io.File

import scala.collection.JavaConverters._

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval.Task

class Node(conf: Conf) {

  val logger = Logger("main")
  def whoami(port: Int): Option[InetAddress] = {

    val upnp = new UPnP(port)

    logger.info(s"uPnP: ${upnp.localAddress} -> ${upnp.externalAddress}")

    upnp.localAddress match {
      case Some(addy) => Some(addy)
      case None => {
        val ifaces = NetworkInterface.getNetworkInterfaces.asScala.map(_.getInterfaceAddresses)
        val addresses = ifaces
          .flatMap(_.asScala)
          .map(_.getAddress)
          .toList
          .groupBy(x => x.isLoopbackAddress || x.isLinkLocalAddress || x.isSiteLocalAddress)
        if (addresses.contains(false)) {
          Some(addresses(false).head)
        } else {
          val locals = addresses(true).groupBy(x => x.isLoopbackAddress || x.isLinkLocalAddress)
          if (locals.contains(false)) {
            Some(locals(false).head)
          } else if (locals.contains(true)) {
            Some(locals(true).head)
          } else {
            None
          }
        }
      }
    }
  }

  val name = conf.name.toOption match {
    case Some(key) => key
    case None      => UUID.randomUUID.toString.replaceAll("-", "")
  }

  val host = conf.host.toOption match {
    case Some(host) => host
    case None       => whoami(conf.port()).fold("localhost")(_.getHostAddress)
  }

  val address = s"rnode://$name@$host:${conf.port()}"
  val src     = p2p.NetworkAddress.parse(address).right.get

  import ApplicativeError_._

  val metricsServer = MetricsServer()

  val http = HttpServer(conf.httpPort())
  http.start

  val remoteKeysPath = System.getProperty("user.home") + File.separator + s".${name}-rnode-remote.keys"

  val net = new UnicastNetwork(src, Some(p2p.Network))

  /** This is essentially a final effect that will accumulate all effects from the system */
  type CommErrT[F[_], A] = EitherT[F, CommError, A]
  type Effect[A]         = CommErrT[Task, A]

  implicit class EitherEffectOps[A](e: Either[CommError, A]) {
    def toEffect: Effect[A] = EitherT[Task, CommError, A](e.pure[Task])
  }
  implicit class TaskEffectOps[A](t: Task[A]) {
    def toEffect: Effect[A] = t.liftM[CommErrT]
  }

  implicit val encryptionEffect: Encryption[Task]        = effects.encryption(name)
  implicit val logEffect: Log[Task]                      = effects.log
  implicit val timeEffect: Time[Task]                    = effects.time
  implicit val metricsEffect: Metrics[Task]              = effects.metrics
  implicit val inMemoryPeerKeysEffect: KeysStore[Task]   = effects.remoteKeysKvs(remoteKeysPath)
  implicit val communicatonEffect: Communication[Effect] = effects.communication[Effect](net)

  def connectToBootstrap: Effect[Unit] =
    for {
      bootstrapAddrStr <- conf.bootstrap.toOption
                           .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
                           .toEffect
      bootstrapAddr <- p2p.NetworkAddress.parse(bootstrapAddrStr).toEffect
      _             <- Log[Effect].info(s"Bootstrapping from $bootstrapAddr.")
      _             <- p2p.Network.connect[Effect](bootstrapAddr)
      _             <- Log[Effect].info(s"Connected $bootstrapAddr.")
    } yield ()

  def addShutdownHook: Task[Unit] = Task.delay {
    sys.addShutdownHook {
      metricsServer.stop
      http.stop
      net.broadcast(
        DisconnectMessage(ProtocolMessage.disconnect(net.local), System.currentTimeMillis))
      logger.info("Goodbye.")
    }
  }

  def findAndConnect: Int => Effect[Int] = {

    val err: ApplicativeError_[Effect, CommError] = ApplicativeError_[Effect, CommError]

    (lastCount: Int) =>
      (for {
        _     <- IOUtil.sleep[Effect](5000L)
        peers <- Communication[Effect].findMorePeers(10)
        _ <- peers.toList.traverse(p => err.attempt(p2p.Network.connect[Effect](p))).map {
              attempts =>
                attempts.filter {
                  case Left(_) => false
                  case _       => true
                }
            }
        thisCount <- Communication[Effect].countPeers
        _ <- if (thisCount != lastCount) Log[Effect].info(s"Peers: $thisCount.")
            else ().pure[Effect]
      } yield thisCount)
  }

  val recipe: Effect[Unit] = for {
    _ <- Task.fork(MonadOps.forever(net.receiver[Effect].value.void)).start.toEffect
    _ <- addShutdownHook.toEffect
    _ <- Log[Effect].info(s"Listening for traffic on $address.")
    _ <- if (conf.standalone()) Log[Effect].info(s"Starting stand-alone node.")
        else connectToBootstrap
    _ <- MonadOps.forever(findAndConnect, 0)
  } yield ()

}
