package coop.rchain.node

import org.rogach.scallop._
import java.util.UUID
import java.net.{InetAddress, NetworkInterface}
import scala.collection.JavaConverters._
import coop.rchain.p2p
import coop.rchain.comm._
import coop.rchain.catscontrib.Capture
import com.typesafe.scalalogging.Logger
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Await
import scala.concurrent.duration._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

import kamon._

object TaskContrib {
  implicit class TaskOps[A](task: Task[A])(implicit scheduler: Scheduler) {
    def unsafeRunSync(handle: A => Unit): Unit =
      Await.result(task.runAsync, Duration.Inf)
  }
}

final case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("RChain Node version 0.1")

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

    import encryption._

    val conf = Conf(args)

    val name = conf.name.toOption match {
      case Some(key) => key
      case None      => UUID.randomUUID.toString.replaceAll("-", "")
    }

    val host = conf.host.toOption match {
      case Some(host) => host
      case None       => whoami(conf.port()).fold("localhost")(_.getHostAddress)
    }

    import ApplicativeError_._

    implicit def ioLog: Log[Task] = new Log[Task] {

      def debug(msg: String): Task[Unit] = Task.delay(logger.debug(msg))
      def info(msg: String): Task[Unit]  = Task.delay(logger.info(msg))
      def warn(msg: String): Task[Unit]  = Task.delay(logger.warn(msg))
      def error(msg: String): Task[Unit] = Task.delay(logger.error(msg))
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
    implicit def inMemoryPeerKeys: Kvs[Task, PeerNode, Array[Byte]] =
      new Kvs[Task, PeerNode, Array[Byte]] {
        var mem: Map[PeerNode, Array[Byte]] = Map.empty[PeerNode, Array[Byte]]

        def keys: Task[Vector[PeerNode]] = Task.delay {
          mem.keys.toVector
        }
        def get(k: PeerNode): Task[Option[Array[Byte]]] = Task.delay {
          mem.get(k)
        }
        def put(k: PeerNode, v: Array[Byte]): Task[Unit] = Task.delay {
          mem = mem + (k -> v)
        }

        def delete(k: PeerNode): Task[Unit] = Task.delay {
          mem = mem - k
        }
      }

    /** This is essentially a final effect that will accumulate all effects from the system */
    type CommErrT[F[_], A] = EitherT[F, CommError, A]
    type Effect[A]         = CommErrT[Task, A]

    implicit class EitherEffectOps[A](e: Either[CommError, A]) {
      def toEffect: Effect[A] = EitherT[Task, CommError, A](e.pure[Task])
    }

    implicit class TaskEffectOps[A](t: Task[A]) {
      def toEffect: Effect[A] = t.liftM[CommErrT]
    }

    val metricsServer = MetricsServer()

    val http = HttpServer(conf.httpPort())
    http.start

    val calculateKeys: Effect[PublicPrivateKeys] = for {
      inDb <- keysAvailable[Effect]
      ks   <- if (inDb) fetchKeys[Effect] else generate.pure[Effect]
    } yield ks

    // move to p2p.Network
    def connectToBootstrap(net: p2p.Network): Effect[Unit] =
      for {
        bootstrapAddrStr <- conf.bootstrap.toOption
                             .fold[Either[CommError, String]](Left(BootstrapNotProvided))(Right(_))
                             .toEffect
        bootstrapAddr <- p2p.NetworkAddress.parse(bootstrapAddrStr).toEffect
        _             <- Log[Effect].info(s"Bootstrapping from $bootstrapAddr.")
        _             <- net.connect[Effect](bootstrapAddr)
        _             <- Log[Effect].info(s"Connected $bootstrapAddr.")
      } yield ()

    def addShutdownHook(net: p2p.Network): Task[Unit] = Task.delay {
      sys.addShutdownHook {
        metricsServer.stop
        http.stop
        net.disconnect
        logger.info("Goodbye.")
      }
    }

    def findAndConnect(net: p2p.Network): Long => Effect[Long] = {

      val err: ApplicativeError_[Effect, CommError] = ApplicativeError_[Effect, CommError]

      (lastCount: Long) =>
        (for {
          _ <- IOUtil.sleep[Effect](5000L)
          peers <- Task.delay { // TODO lift findMorePeers to return IO
                    net.net.findMorePeers(limit = 10)
                  }.toEffect
          _ <- peers.toList.traverse(p => err.attempt(net.connect[Effect](p))).map { attempts =>
                attempts.filter {
                  case Left(err) => false
                  case right     => true
                }
              }
          tc <- Task.delay { // TODO refactor once findMorePeers return IO
                 val thisCount = net.net.table.peers.size
                 if (thisCount != lastCount) {
                   logger.info(s"Peers: $thisCount.")
                 }
                 thisCount
               }.toEffect
        } yield tc)

    }

    val recipe: Effect[Unit] = for {
      addy <- p2p.NetworkAddress.parse(s"rnode://$name@$host:${conf.port()}").toEffect
      keys <- calculateKeys
      net  <- (new p2p.Network(addy, keys)).pure[Effect]
      _    <- Task.fork(MonadOps.forever(net.net.receiver[Effect].value.void)).start.toEffect
      _    <- addShutdownHook(net).toEffect
      _    <- Log[Effect].info(s"Listening for traffic on $net.")
      _ <- if (conf.standalone()) Log[Effect].info(s"Starting stand-alone node.")
          else connectToBootstrap(net)
      _ <- MonadOps.forever(findAndConnect(net), 0L)
    } yield ()

    import monix.execution.Scheduler.Implicits.global
    import TaskContrib._
    recipe.value.unsafeRunSync {
      case Right(_) => ()
      case Left(commError) =>
        throw new Exception(commError.toString) // TODO use Show instance instead
    }

  }
}
