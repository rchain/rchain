package coop.rchain.node

import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p, p2p.NetworkAddress, p2p.Network.KeysStore
import coop.rchain.p2p.effects._
import coop.rchain.comm._, CommError._
import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._, ski._, TaskContrib._
import monix.eval.Task

object effects {
  def encryption(nodeName: String): Encryption[Task] = new Encryption[Task] {
    import Encryption._
    import coop.rchain.crypto.encryption.Curve25519
    import com.google.common.io.BaseEncoding

    val encoder = BaseEncoding.base16().lowerCase()

    private def generateFresh: Task[PublicPrivateKeys] = Task.delay {
      val (pub, sec) = Curve25519.newKeyPair
      PublicPrivateKeys(pub, sec)
    }

    val storePath = System.getProperty("user.home") + File.separator + s".${nodeName}-rnode.keys"

    private def storeToFS: PublicPrivateKeys => Task[Unit] =
      keys =>
        Task
          .delay {
            val pw = new PrintWriter(new File(storePath))
            pw.println(encoder.encode(keys.pub))
            pw.println(encoder.encode(keys.priv))
            pw.close
          }
          .attempt
          .void

    private def fetchFromFS: Task[Option[PublicPrivateKeys]] =
      Task
        .delay {
          val lines  = scala.io.Source.fromFile(storePath).getLines.toList
          val pubKey = encoder.decode(lines(0))
          val secKey = encoder.decode(lines(1))
          PublicPrivateKeys(pubKey, secKey)
        }
        .attempt
        .map(_.toOption)

    def fetchKeys: Task[PublicPrivateKeys] =
      (fetchFromFS >>= {
        case None     => generateFresh >>= (keys => (storeToFS(keys) *> keys.pure[Task]))
        case Some(ks) => ks.pure[Task]
      }).memoize

    def generateNonce: Task[Nonce] = Task.delay(Curve25519.newNonce)

    def encrypt(pub: Key, sec: Key, nonce: Nonce, message: Array[Byte]): Task[Array[Byte]] =
      Task.delay(Curve25519.encrypt(pub, sec, nonce, message))

    def decrypt(pub: Key, sec: Key, nonce: Nonce, cipher: Array[Byte]): Task[Array[Byte]] =
      Task.delay(Curve25519.decrypt(pub, sec, nonce, cipher))
  }

  def log: Log[Task] = new Log[Task] {
    import com.typesafe.scalalogging.Logger

    val logger = Logger("logger")

    def debug(msg: String): Task[Unit] = Task.delay(logger.debug(msg))
    def info(msg: String): Task[Unit]  = Task.delay(logger.info(msg))
    def warn(msg: String): Task[Unit]  = Task.delay(logger.warn(msg))
    def error(msg: String): Task[Unit] = Task.delay(logger.error(msg))
  }

  def time: Time[Task] = new Time[Task] {
    def currentMillis: Task[Long] = Task.delay {
      System.currentTimeMillis
    }
    def nanoTime: Task[Long] = Task.delay {
      System.nanoTime
    }
  }

  def metrics: Metrics[Task] = new Metrics[Task] {
    import kamon._

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

  def remoteKeysKvs(path: String): Kvs[Task, PeerNode, Array[Byte]] =
    new Kvs[Task, PeerNode, Array[Byte]] {
      import com.google.protobuf.ByteString
      var m: Map[PeerNode, Array[Byte]] = fetch()

      def keys: Task[Vector[PeerNode]]                = Task.delay(m.keys.toVector)
      def get(k: PeerNode): Task[Option[Array[Byte]]] = Task.delay(m.get(k))
      def put(k: PeerNode, v: Array[Byte]): Task[Unit] = Task.delay {
        m = m + (k -> v)
        store()
      }

      def delete(k: PeerNode): Task[Unit] = Task.delay {
        m = m - k
        store()
      }

      private def fetch(): Map[PeerNode, Array[Byte]] = {
        val file = new File(path)
        file.createNewFile()
        KeysStore
          .parseFrom(new FileInputStream(file))
          .keys
          .map {
            case (k, v) => (NetworkAddress.parse(k), v)
          }
          .flatMap {
            case (Right(peerNode), v) => Map(peerNode -> v.toByteArray)
            case (Left(_), _)         => Map.empty[PeerNode, Array[Byte]]
          }
      }

      private def store(): Unit =
        KeysStore(m.map {
          case (k, v) => (k.toAddress, ByteString.copyFrom(v))
        }).writeTo(new FileOutputStream(new File(path)))
    }

  def communication[F[_]: Monad: Capture: Metrics](net: UnicastNetwork): Communication[F] =
    new Communication[F] {
      import scala.concurrent.duration._

      def roundTrip(msg: ProtocolMessage,
                    remote: ProtocolNode,
                    timeout: Duration): F[CommErr[ProtocolMessage]] =
        net.roundTrip[F](msg, remote, timeout)
      def local: F[ProtocolNode] = net.local.pure[F]
      def commSend(msg: ProtocolMessage, peer: PeerNode): F[CommErr[Unit]] =
        Capture[F].capture(net.comm.send(msg.toByteSeq, peer))
      def addNode(node: PeerNode): F[Unit] =
        for {
          _ <- Capture[F].capture(net.add(node))
          _ <- Metrics[F].incrementCounter("peers")
        } yield ()
      def broadcast(msg: ProtocolMessage): F[Seq[CommErr[Unit]]] =
        Capture[F].capture {
          net.broadcast(msg)
        }
      def findMorePeers(limit: Int): F[Seq[PeerNode]] =
        Capture[F].capture {
          net.findMorePeers(limit)
        }
      def peers: F[Seq[PeerNode]] =
        Capture[F].capture {
          net.table.peers
        }
    }

  def packetHandler[F[_]: Applicative: Log](
      pf: PartialFunction[Packet, F[String]]): PacketHandler[F] =
    new PacketHandler[F] {
      def handlePacket(packet: Packet): F[String] = {
        val errorMsg = s"Unable to handle packet $packet"
        if (pf.isDefinedAt(packet)) pf(packet) else Log[F].error(errorMsg) *> errorMsg.pure[F]
      }
    }
}
