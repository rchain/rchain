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
    type Effect[A] = EitherT[Task, CommError, A]

    implicit class EitherOps[A](e: Either[CommError, A]) {
      def toEffect: Effect[A] = EitherT[Task, CommError, A](e.pure[Task])
    }

    val http = HttpServer(8080)
    http.start

    val calculateKeys: Effect[PublicPrivateKeys] = for {
      inDb <- keysAvailable[Effect]
      ks   <- if (inDb) fetchKeys[Effect] else generate.pure[Effect]
    } yield ks

    val recipe: Effect[Unit] = for {
      addy <- p2p.NetworkAddress.parse(s"rnode://$name@$host:${conf.port()}").toEffect
      keys <- calculateKeys
    } yield {
      val net = p2p.Network(addy, keys)
      logger.info(s"Listening for traffic on $net.")

      if (!conf.standalone()) {
        conf.bootstrap.toOption.flatMap(p2p.NetworkAddress.parse _ andThen (_.toOption)).foreach {
          address =>
            logger.info(s"Bootstrapping from $address.")
            net.connect(address)
        }
      } else {
        logger.info(s"Starting stand-alone node.")
      }

      sys.addShutdownHook {
        net.disconnect
        logger.info("Goodbye.")
      }

      val pauseTime = 5000L
      var lastCount = 0
      while (true) {
        Thread.sleep(pauseTime)
        for (peer <- net.net.findMorePeers(10)) {
          logger.info(s"Possibly new peer: $peer.")
          net.connect(peer)
        }
        val thisCount = net.net.table.peers.size
        if (thisCount != lastCount) {
          lastCount = thisCount
          logger.info(s"Peers: $thisCount.")
        }
      }
    }

    import monix.execution.Scheduler.Implicits.global
    import TaskContrib._
    recipe.value.unsafeRunSync {
      case Right(_) => ()
      case Left(commError) =>
        throw new Exception(commError.toString) // TODO use Show instance instead
    }

  }
}
