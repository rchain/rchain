package coop.rchain.node

import org.rogach.scallop._
import java.util.UUID
import java.net.{InetAddress, NetworkInterface}
import scala.collection.JavaConverters._
import coop.rchain.p2p
import coop.rchain.comm._, CommError._
import coop.rchain.catscontrib.Capture
import com.typesafe.scalalogging.Logger
import com.google.common.io.BaseEncoding
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Await
import scala.concurrent.duration._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._

import kamon._

final case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version(s"RChain Node ${BuildInfo.version}")

  val name =
    opt[String](default = None, short = 'n', descr = "Node name or key.")

  val port =
    opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

  val httpPort =
    opt[Int](default = Some(8080), short = 'x', descr = "HTTP port.")

  val bootstrap =
    opt[String](default = Some("rnode://0f365f1016a54747b384b386b8e85352@216.83.154.106:30012"),
                short = 'b',
                descr = "Bootstrap rnode address for initial seed.")

  val standalone = opt[Boolean](default = Some(false),
                                short = 's',
                                descr = "Start a stand-alone node (no bootstrapping).")

  val host = opt[String](default = None, descr = "Hostname or IP of this node.")

  verify()
}

object Main {
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

  def main(args: Array[String]): Unit = {

    val conf = Conf(args)

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

    // FIX-ME temp implemmentation, production shoul use crypto module
    implicit val encyption: Encryption[Task] = new Encryption[Task] {
      import Encryption._
      val encoder = BaseEncoding.base16().lowerCase()
      def fetchKeys: Task[PublicPrivateKeys] = Task.delay {
        val pub: Key =
          encoder.decode("de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f")
        val sec: Key =
          encoder.decode("5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
        PublicPrivateKeys(pub, sec)
      }

      def generateNonce: Task[Nonce] = Task.delay {
        encoder.decode("69696ee955b62b73cd62bda875fc73d68219e0036b7a0b37")
      }

      def encrypt(pub: Key, sec: Key, nonce: Nonce, message: Array[Byte]): Task[Array[Byte]] =
        Task.delay {
          message
        }
      def decrypt(pub: Key, sec: Key, nonce: Nonce, cipher: Array[Byte]): Task[Array[Byte]] =
        Task.delay {
          cipher
        }
    }

    implicit def ioLog: Log[Task] = new Log[Task] {
      def debug(msg: String): Task[Unit] = Task.delay(logger.debug(msg))
      def info(msg: String): Task[Unit]  = Task.delay(logger.info(msg))
      def warn(msg: String): Task[Unit]  = Task.delay(logger.warn(msg))
      def error(msg: String): Task[Unit] = Task.delay(logger.error(msg))
    }

    implicit def time: Time[Task] = new Time[Task] {
      def currentMillis: Task[Long] = Task.delay {
        System.currentTimeMillis
      }
      def nanoTime: Task[Long] = Task.delay {
        System.nanoTime
      }
    }

    implicit def metrics: Metrics[Task] = new Metrics[Task] {
      val m = scala.collection.concurrent.TrieMap[String, metric.Metric[_]]()

      def incrementCounter(name: String, delta: Long): Task[Unit] = Task.delay {
        m.getOrElseUpdate(name, { Kamon.counter(name) }) match {
          case (c: metric.Counter) => c.increment(delta)
        }
      }

      def incrementSampler(name: String, delta: Long): Task[Unit] = Task.delay {
        m.getOrElseUpdate(name, { Kamon.rangeSampler(name) }) match {
          case (c: metric.RangeSampler) => c.increment(delta)
        }
      }

      def sample(name: String): Task[Unit] = Task.delay {
        m.getOrElseUpdate(name, { Kamon.rangeSampler(name) }) match {
          case (c: metric.RangeSampler) => c.sample
        }
      }

      def setGauge(name: String, value: Long): Task[Unit] = Task.delay {
        m.getOrElseUpdate(name, { Kamon.gauge(name) }) match {
          case (c: metric.Gauge) => c.set(value)
        }
      }

      def record(name: String, value: Long, count: Long = 1): Task[Unit] = Task.delay {
        m.getOrElseUpdate(name, { Kamon.histogram(name) }) match {
          case (c: metric.Histogram) => c.record(value, count)
        }
      }
    }

    /** will use database or file system */
    implicit val inMemoryPeerKeys: Kvs[Task, PeerNode, Array[Byte]] =
      new Kvs.InMemoryKvs[Task, PeerNode, Array[Byte]]

    /** This is essentially a final effect that will accumulate all effects from the system */
    type CommErrT[F[_], A] = EitherT[F, CommError, A]
    type Effect[A]         = CommErrT[Task, A]

    implicit class EitherEffectOps[A](e: Either[CommError, A]) {
      def toEffect: Effect[A] = EitherT[Task, CommError, A](e.pure[Task])
    }

    implicit class TaskEffectOps[A](t: Task[A]) {
      def toEffect: Effect[A] = t.liftM[CommErrT]
    }

    val net = new UnicastNetwork(src, Some(p2p.Network))

    implicit lazy val communication: Communication[Effect] = new Communication[Effect] {
      def roundTrip(
          msg: ProtocolMessage,
          remote: ProtocolNode,
          timeout: Duration = Duration(500, MILLISECONDS)): Effect[CommErr[ProtocolMessage]] =
        net.roundTrip[Effect](msg, remote, timeout)
      def local: Effect[ProtocolNode] = net.local.pure[Effect]
      def commSend(msg: ProtocolMessage, peer: PeerNode): Effect[CommErr[Unit]] =
        Task.delay(net.comm.send(msg.toByteSeq, peer)).toEffect
      def addNode(node: PeerNode): Effect[Unit] =
        for {
          _ <- Task.delay(net.add(node)).toEffect
          _ <- Metrics[Effect].incrementCounter("peers")
        } yield ()
      def broadcast(msg: ProtocolMessage): Effect[Seq[CommErr[Unit]]] =
        Task.delay {
          net.broadcast(msg)
        }.toEffect
      def findMorePeers(limit: Int): Effect[Seq[PeerNode]] =
        Task.delay {
          net.findMorePeers(limit)
        }.toEffect
      def countPeers: Effect[Int] =
        Task.delay {
          net.table.peers.size
        }.toEffect
      def receiver: Effect[Unit] = net.receiver[Effect]
    }

    val metricsServer = MetricsServer()

    val http = HttpServer(conf.httpPort())
    http.start

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
      _ <- Task.fork(MonadOps.forever(Communication[Effect].receiver.value.void)).start.toEffect
      _ <- addShutdownHook.toEffect
      _ <- Log[Effect].info(s"Listening for traffic on $address.")
      _ <- if (conf.standalone()) Log[Effect].info(s"Starting stand-alone node.")
          else connectToBootstrap
      _ <- MonadOps.forever(findAndConnect, 0)
    } yield ()

    import monix.execution.Scheduler.Implicits.global
    recipe.value.unsafeRunSync {
      case Right(_) => ()
      case Left(commError) =>
        throw new Exception(commError.toString) // TODO use Show instance instead
    }

  }
}
