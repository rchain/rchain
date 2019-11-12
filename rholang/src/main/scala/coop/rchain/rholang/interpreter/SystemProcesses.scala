package coop.rchain.rholang.interpreter

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.{Blake2b256, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.metrics.Span
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.{BlockData, InvalidBlocks, RhoTuplespace}
import coop.rchain.rholang.interpreter.registry.Registry
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.{ContResult, Result}

import scala.util.Try

//TODO: Make each of the system processes into a case class,
//      so that implementation is not repetitive.
//TODO: Make polymorphic over match type.
trait SystemProcesses[F[_]] {
  import SystemProcesses.Contract

  def stdOut: Contract[F]
  def stdOutAck: Contract[F]
  def stdErr: Contract[F]
  def stdErrAck: Contract[F]
  def secp256k1Verify: Contract[F]
  def ed25519Verify: Contract[F]
  def sha256Hash: Contract[F]
  def keccak256Hash: Contract[F]
  def blake2b256Hash: Contract[F]
  def getBlockData(blockData: Ref[F, BlockData]): Contract[F]
  def invalidBlocks(invalidBlocks: InvalidBlocks[F]): Contract[F]
  def revAddress: Contract[F]
  def deployerIdOps: Contract[F]
  def registryOps: Contract[F]
}

object SystemProcesses {
  type Contract[F[_]] = Seq[ListParWithRandom] => F[Unit]

  def apply[F[_]](
      dispatcher: Dispatch[F, ListParWithRandom, TaggedContinuation],
      space: RhoTuplespace[F]
  )(implicit F: Concurrent[F], spanF: Span[F]): SystemProcesses[F] =
    new SystemProcesses[F] {

      type ContWithMetaData = ContResult[Par, BindPattern, TaggedContinuation]

      type Channels = Seq[Result[Par, ListParWithRandom]]

      private val prettyPrinter = PrettyPrinter()

      private val isContractCall = new ContractCall[F](space, dispatcher)

      private val stdOutLogger = Logger("coop.rchain.rholang.stdout")
      private val stdErrLogger = Logger("coop.rchain.rholang.stderr")

      private def illegalArgumentException(msg: String): F[Unit] =
        F.raiseError(new IllegalArgumentException(msg))

      def verifySignatureContract(
          name: String,
          algorithm: (Array[Byte], Array[Byte], Array[Byte]) => Boolean
      ): Contract[F] = {
        case isContractCall(
            produce,
            Seq(
              RhoType.ByteArray(data),
              RhoType.ByteArray(signature),
              RhoType.ByteArray(pub),
              ack
            )
            ) =>
          for {
            verified <- F.fromTry(Try(algorithm(data, signature, pub)))
            _        <- produce(Seq(RhoType.Boolean(verified)), ack)
          } yield ()
        case _ =>
          illegalArgumentException(
            s"$name expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
          )
      }

      def hashContract(name: String, algorithm: Array[Byte] => Array[Byte]): Contract[F] = {
        case isContractCall(produce, Seq(RhoType.ByteArray(input), ack)) =>
          for {
            hash <- F.fromTry(Try(algorithm(input)))
            _    <- produce(Seq(RhoType.ByteArray(hash)), ack)
          } yield ()
        case _ =>
          illegalArgumentException(
            s"$name expects a byte array and return channel"
          )
      }

      private def printStdOut(s: String): F[Unit] =
        for {
          _ <- F.delay(Console.println(s))
          _ <- F.delay(stdOutLogger.debug(s))
        } yield ()

      private def printStdErr(s: String): F[Unit] =
        for {
          _ <- F.delay(Console.err.println(s))
          _ <- F.delay(stdErrLogger.debug(s))
        } yield ()

      def stdOut: Contract[F] = {
        case isContractCall(_, Seq(arg)) =>
          printStdOut(prettyPrinter.buildString(arg))
      }

      def stdOutAck: Contract[F] = {
        case isContractCall(produce, Seq(arg, ack)) =>
          for {
            _ <- printStdOut(prettyPrinter.buildString(arg))
            _ <- produce(Seq(Par.defaultInstance), ack)
          } yield ()
      }

      def stdErr: Contract[F] = {
        case isContractCall(_, Seq(arg)) =>
          printStdErr(prettyPrinter.buildString(arg))
      }

      def stdErrAck: Contract[F] = {
        case isContractCall(produce, Seq(arg, ack)) =>
          for {
            _ <- printStdErr(prettyPrinter.buildString(arg))
            _ <- produce(Seq(Par.defaultInstance), ack)
          } yield ()
      }

      def revAddress: Contract[F] = {
        case isContractCall(
            produce,
            Seq(RhoType.String("validate"), RhoType.String(address), ack)
            ) =>
          val errorMessage =
            RevAddress
              .parse(address)
              .swap
              .toOption
              .map(RhoType.String(_))
              .getOrElse(Par())

          produce(Seq(errorMessage), ack)

        case isContractCall(produce, Seq(RhoType.String("validate"), _, ack)) =>
          produce(Seq(Par()), ack)

        case isContractCall(
            produce,
            Seq(RhoType.String("fromPublicKey"), RhoType.ByteArray(publicKey), ack)
            ) =>
          val response =
            RevAddress
              .fromPublicKey(PublicKey(publicKey))
              .map(ra => RhoType.String(ra.toBase58))
              .getOrElse(Par())

          produce(Seq(response), ack)

        case isContractCall(produce, Seq(RhoType.String("fromPublicKey"), _, ack)) =>
          produce(Seq(Par()), ack)

        case isContractCall(
            produce,
            Seq(RhoType.String("fromDeployerId"), RhoType.DeployerId(id), ack)
            ) =>
          val response =
            RevAddress
              .fromDeployerId(id)
              .map(ra => RhoType.String(ra.toBase58))
              .getOrElse(Par())

          produce(Seq(response), ack)

        case isContractCall(produce, Seq(RhoType.String("fromDeployerId"), _, ack)) =>
          produce(Seq(Par()), ack)

        case isContractCall(
            produce,
            Seq(RhoType.String("fromUnforgeable"), argument, ack)
            ) =>
          val response = argument match {
            case RhoType.Name(gprivate) =>
              RhoType.String(RevAddress.fromUnforgeable(gprivate).toBase58)
            case _ => Par()
          }

          produce(Seq(response), ack)

        case isContractCall(produce, Seq(RhoType.String("fromUnforgeable"), _, ack)) =>
          produce(Seq(Par()), ack)
      }

      def deployerIdOps: Contract[F] = {
        case isContractCall(
            produce,
            Seq(RhoType.String("pubKeyBytes"), RhoType.DeployerId(publicKey), ack)
            ) =>
          produce(Seq(RhoType.ByteArray(publicKey)), ack)

        case isContractCall(produce, Seq(RhoType.String("pubKeyBytes"), _, ack)) =>
          produce(Seq(Par()), ack)
      }

      def registryOps: Contract[F] = {
        case isContractCall(
            produce,
            Seq(RhoType.String("buildUri"), argument, ack)
            ) =>
          val response = argument match {
            case RhoType.ByteArray(ba) =>
              val hashKeyBytes = Blake2b256.hash(ba)
              RhoType.Uri(Registry.buildURI(hashKeyBytes))
            case _ => Par()
          }
          produce(Seq(response), ack)
      }

      def secp256k1Verify: Contract[F] =
        verifySignatureContract("secp256k1Verify", Secp256k1.verify)

      def ed25519Verify: Contract[F] =
        verifySignatureContract("ed25519Verify", Ed25519.verify)

      def sha256Hash: Contract[F] =
        hashContract("sha256Hash", Sha256.hash)

      def keccak256Hash: Contract[F] =
        hashContract("keccak256Hash", Keccak256.hash)

      def blake2b256Hash: Contract[F] =
        hashContract("blake2b256Hash", Blake2b256.hash)

      def getBlockData(
          blockData: Ref[F, BlockData]
      ): Contract[F] = {
        case isContractCall(produce, Seq(ack)) =>
          for {
            data <- blockData.get
            _ <- produce(
                  Seq(
                    RhoType.Number(data.blockNumber),
                    RhoType.Number(data.timeStamp),
                    RhoType.ByteArray(data.sender.bytes)
                  ),
                  ack
                )
          } yield ()
        case _ =>
          illegalArgumentException("blockData expects only a return channel")
      }

      def invalidBlocks(invalidBlocks: Runtime.InvalidBlocks[F]): Contract[F] = {
        case isContractCall(produce, Seq(ack)) =>
          for {
            invalidBlocks <- invalidBlocks.invalidBlocks.get
            _             <- produce(Seq(invalidBlocks), ack)
          } yield ()
        case _ =>
          illegalArgumentException("invalidBlocks expects only a return channel")
      }
    }
}
