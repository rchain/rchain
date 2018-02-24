package coop.rchain.node

import org.rogach.scallop._
import java.util.UUID
import java.net.{InetAddress, NetworkInterface}
import scala.collection.JavaConverters._
import coop.rchain.p2p
import coop.rchain.comm._
import coop.rchain.catscontrib.Capture
import com.typesafe.scalalogging.Logger
import cats._, cats.data._, cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import coop.rchain.catscontrib._, Catscontrib._

object TaskContrib {
  implicit class TaskOps[A](task: Task[A])(implicit scheduler: Scheduler) {
    def unsafeRunSync(handle: A => Unit): Unit =
      // TODO this will eventually disappear
      task.coeval.value match {
        case Left(future) => throw new Exception("could not run in sync")
        case Right(a)     => handle(a)
      }
  }
}

final case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  version("RChain Node version 0.1")

  val name =
    opt[String](default = None, short = 'n', descr = "Node name or key.")

  val port =
    opt[Int](default = Some(30304), short = 'p', descr = "Network port to use.")

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
  val logger   = Logger("main")
  val iologger = IOLogger("main")

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

  /** Those instances have no sens whatsoever however we keep them for now
    * to adjust to imperative side of the code base in main. Once p2p.Network
    * gets a monadic API we will remove this and main will run on a single
    * type that binds all possbile efffects
    */
  object TempInstances {
    implicit val eitherCapture: Capture[Either[CommError, ?]] =
      new Capture[Either[CommError, ?]] {
        def capture[A](a: => A): Either[CommError, A] = Right(a)
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

    /** TODO This is using Either for the effect which obviously is a temp solution. Will use
      * proper type once p2p.Network gets a monadic API
      */
    import TempInstances._

    /** This is essentially a final effect that will accumulate all effects from the system */
    type LogT[F[_], A]     = WriterT[F, Vector[String], A]
    type CommErrT[F[_], A] = EitherT[F, CommError, A]
    type Effect[A]         = CommErrT[LogT[Task, ?], A]

    implicit class EitherEffectOps[A](e: Either[CommError, A]) {
      def toEffect: Effect[A] = EitherT[LogT[Task, ?], CommError, A](e.pure[LogT[Task, ?]])
    }

    implicit class TaskEffectOps[A](t: Task[A]) {
      def toEffect: Effect[A] = t.liftM[LogT].liftM[CommErrT]
    }

    val metrics = Metrics()

    val http = HttpServer(8080)
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
        _             <- iologger.info[Effect](s"Bootstrapping from $bootstrapAddr.")
        _             <- net.connect[Effect](bootstrapAddr)
      } yield ()

    def addShutdownHook(net: p2p.Network): Task[Unit] = Task.delay {
      sys.addShutdownHook {
        metrics.stop
        http.stop
        net.disconnect
        logger.info("Goodbye.")
      }
    }

    def findAndConnect(net: p2p.Network): Long => Task[Long] =
      (lastCount: Long) =>
        (for {
          _ <- IOUtil.sleep[Task](5000L)
          peers <- Task.delay { // TODO lift findMorePeers to return IO
            net.net.findMorePeers(limit = 10)
          }
          _ <- peers.toList.traverse(p => net.connect[Task](p))
          tc <- Task.delay { // TODO refactor once findMorePeers return IO
            val thisCount = net.net.table.peers.size
            if (thisCount != lastCount) {
              logger.info(s"Peers: $thisCount.")
            }
            thisCount
          }
        } yield tc)

    val recipe: Effect[Unit] = for {
      addy <- p2p.NetworkAddress.parse(s"rnode://$name@$host:${conf.port()}").toEffect
      keys <- calculateKeys
      net  <- p2p.Network(addy, keys).pure[Effect]
      _    <- addShutdownHook(net).toEffect
      _    <- iologger.info[Effect](s"Listening for traffic on $net.")
      _ <- if (conf.standalone()) iologger.info[Effect](s"Starting stand-alone node.")
      else connectToBootstrap(net)
      _ <- MonadOps.forever(findAndConnect(net), 0L).toEffect
    } yield ()

    import monix.execution.Scheduler.Implicits.global
    import TaskContrib._
    recipe.value.value.unsafeRunSync {
      case Right(_) => ()
      case Left(commError) =>
        throw new Exception(commError.toString) // TODO use Show instance instead
    }

  }
}
