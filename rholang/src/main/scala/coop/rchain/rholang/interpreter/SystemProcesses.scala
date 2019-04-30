package coop.rchain.rholang.interpreter

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.{Blake2b256, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.{BlockTime, RhoISpace}
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
  def getDeployParams(runtimeParametersRef: Ref[F, DeployParameters]): Contract[F]
  def blockTime(timestamp: BlockTime[F]): Contract[F]
  def validateRevAddress: Contract[F]
}

object SystemProcesses {
  type Contract[F[_]] = (Seq[ListParWithRandom], Int) => F[Unit]

  def apply[F[_]](
      dispatcher: Dispatch[F, ListParWithRandom, TaggedContinuation],
      space: RhoISpace[F]
  )(implicit F: Concurrent[F]): SystemProcesses[F] =
    new SystemProcesses[F] {

      type ContWithMetaData = ContResult[Par, BindPattern, TaggedContinuation]

      type Channels = Seq[Result[ListParWithRandom]]

      private val prettyPrinter = PrettyPrinter()

      private val isContractCall = new ContractCall[F](space, dispatcher)

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

      def stdOut: Contract[F] = {
        case isContractCall(_, Seq(arg)) =>
          F.delay(Console.println(prettyPrinter.buildString(arg)))
      }

      def stdOutAck: Contract[F] = {
        case isContractCall(produce, Seq(arg, ack)) =>
          for {
            _ <- F.delay(Console.println(prettyPrinter.buildString(arg)))
            _ <- produce(Seq(Par.defaultInstance), ack)
          } yield ()
      }

      def stdErr: Contract[F] = {
        case isContractCall(_, Seq(arg)) =>
          F.delay(Console.err.println(prettyPrinter.buildString(arg)))
      }

      def stdErrAck: Contract[F] = {
        case isContractCall(produce, Seq(arg, ack)) =>
          for {
            _ <- F.delay(Console.err.println(prettyPrinter.buildString(arg)))
            _ <- produce(Seq(Par.defaultInstance), ack)
          } yield ()
      }

      def validateRevAddress: Contract[F] = {
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

      // TODO: rename this system process to "deployParameters"?
      def getDeployParams(runtimeParametersRef: Ref[F, DeployParameters]): Contract[F] = {
        case isContractCall(produce, Seq(ack)) =>
          for {
            params                                                  <- runtimeParametersRef.get
            DeployParameters(codeHash, phloRate, userId, timestamp) = params
            _ <- produce(
              Seq(codeHash, phloRate, userId, timestamp),
                  ack
                )
          } yield ()
        case _ =>
          illegalArgumentException("deployParameters expects only a return channel")
      }

      def blockTime(
          blocktime: Runtime.BlockTime[F]
      ): Contract[F] = {
        case isContractCall(produce, Seq(ack)) =>
          for {
            time <- blocktime.timestamp.get
            _    <- produce(Seq(time), ack)
          } yield ()
        case _ =>
          illegalArgumentException("blockTime expects only a return channel")
      }
    }
}
