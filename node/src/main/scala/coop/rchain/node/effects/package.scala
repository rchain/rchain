package coop.rchain.node

import coop.rchain.comm.protocol.rchain.Packet
import coop.rchain.p2p, p2p.NetworkAddress
import coop.rchain.p2p.effects._
import coop.rchain.comm._, CommError._
import coop.rchain.metrics.Metrics
import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Path}

import scala.tools.jline.console._, completer.StringsCompleter

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval.Task
import scala.concurrent.ExecutionContext

package object effects {
  private def createDirectoryIfNotExists(path: Path): Path =
    if (Files.notExists(path)) Files.createDirectory(path) else path

  /** DEPRECATED - will be removed once TLS is working */
  def encryption(keysPath: Path): Encryption[Task] = new Encryption[Task] {
    import Encryption._
    import coop.rchain.crypto.encryption.Curve25519
    import com.google.common.io.BaseEncoding

    val encoder = BaseEncoding.base16().lowerCase()

    private def generateFresh: Task[PublicPrivateKeys] = Task.delay {
      val (pub, sec) = Curve25519.newKeyPair
      PublicPrivateKeys(pub, sec)
    }

    private def storeToFS: PublicPrivateKeys => Task[Unit] =
      keys =>
        Task
          .delay {
            createDirectoryIfNotExists(keysPath.getParent)
            val pw = new PrintWriter(keysPath.toFile)
            pw.println(encoder.encode(keys.pub))
            pw.println(encoder.encode(keys.priv))
            pw.close()
          }
          .attempt
          .void

    private def fetchFromFS: Task[Option[PublicPrivateKeys]] =
      Task
        .delay {
          val lines  = scala.io.Source.fromFile(keysPath.toFile).getLines.toList
          val pubKey = encoder.decode(lines(0))
          val secKey = encoder.decode(lines(1))
          PublicPrivateKeys(pubKey, secKey)
        }
        .attempt
        .map(_.toOption)

    def fetchKeys: Task[PublicPrivateKeys] =
      (fetchFromFS >>= {
        case None     => generateFresh >>= (keys => storeToFS(keys) *> keys.pure[Task])
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

  def remoteKeysKvs(remoteKeysPath: Path): Kvs[Task, PeerNode, Array[Byte]] =
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
        createDirectoryIfNotExists(remoteKeysPath.getParent)
        val file = remoteKeysPath.toFile
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
        }).writeTo(new FileOutputStream(remoteKeysPath.toFile))
    }

  def ping[F[_]: Monad: Capture: Metrics: TransportLayer](src: PeerNode): Ping[F] =
    new Ping[F] {
      import scala.concurrent.duration._
      def ping(node: PeerNode): F[Boolean] =
        for {
          _   <- Metrics[F].incrementCounter("protocol-ping-sends")
          req = PingMessage(ProtocolMessage.ping(src), System.currentTimeMillis)
          res <- TransportLayer[F].roundTrip(req, node, 500.milliseconds).map(_.toOption)
        } yield res.isDefined
    }

  def tcpTranposrtLayer[F[_]: Monad: Capture: Metrics: Futurable](conf: Conf)(src: PeerNode)(
      implicit executionContext: ExecutionContext) =
    new TcpTransportLayer[F](conf.localhost,
                             conf.port(),
                             conf.certificatePath.toFile,
                             conf.keyPath.toFile)(src)

  def consoleIO(consoleReader: ConsoleReader): ConsoleIO[Task] = new JLineConsoleIO(consoleReader)
}
