package coop.rchain.rholang.interpreter

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.{Blake2b256, Blake2b512Random, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray, GString}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.ShortLeashParams.ShortLeashParameters
import coop.rchain.rholang.interpreter.Runtime.{BlockTime, RhoISpace, ShortLeashParams}
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.{InterpreterError, OutOfPhlogistonsError}
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.util._
import coop.rchain.rspace.{ContResult, Result}

import scala.util.Try

//TODO: Make each of the system processes into a case class,
//      so that implementation is not repetitive.
//TODO: Make polymorphic over match type.
trait SystemProcesses[F[_]] {
  def stdOut: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def stdOutAck: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def stdErr: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def stdErrAck: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def secp256k1Verify: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def ed25519Verify: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def sha256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def keccak256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def blake2b256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def getDeployParams(
      deployParameters: ShortLeashParams[F]
  ): (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def blockTime(timestamp: BlockTime[F]): (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
  def validateRevAddress: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]
}

object RhoType {
  object ByteArray {
    import coop.rchain.models.rholang.implicits._
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleExpr().collect {
        case Expr(GByteArray(bs)) => bs.toByteArray
      }

    def apply(bytes: Array[Byte]): Par =
      Expr(GByteArray(ByteString.copyFrom(bytes)))
  }

  object String {
    def unapply(p: Par): Option[String] =
      p.singleExpr().collect {
        case Expr(GString(bs)) => bs
      }

    def apply(s: String): Par = GString(s)
  }

  object Bool {
    def apply(b: Boolean) = Expr(GBool(b))
  }
}

object SystemProcesses {
  def apply[F[_]](
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation],
      space: RhoISpace[F]
  )(implicit F: Sync[F]): SystemProcesses[F] =
    new SystemProcesses[F] {

      type ContWithMetaData = ContResult[Par, BindPattern, TaggedContinuation]

      type Channels = Seq[Result[ListParWithRandomAndPhlos]]

      private val prettyPrinter = PrettyPrinter()

      private val isContractCall = new ContractCall[F](space, dispatcher)

      private def illegalArgumentException(msg: String): F[Unit] =
        F.raiseError(new IllegalArgumentException(msg))

      def verifySignatureContract(name : String,
                                  algorithm : (Array[Byte], Array[Byte], Array[Byte]) => Boolean)
      : (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
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
            _        <- produce(Seq(RhoType.Bool(verified)), ack)
          } yield ()
        case _ =>
          illegalArgumentException(
            s"$name expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
          )
      }

      def hashContract(name : String,
                       algorithm : Array[Byte] => Array[Byte])
      : (Seq[ListParWithRandomAndPhlos], Int) => F[Unit]= {
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

      def stdOut: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case isContractCall(_, Seq(arg)) =>
          F.delay(Console.println(prettyPrinter.buildString(arg)))
      }

      def stdOutAck: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case isContractCall(produce, Seq(arg, ack)) =>
          for {
            _ <- F.delay(Console.println(prettyPrinter.buildString(arg)))
            _ <- produce(Seq(Par.defaultInstance), ack)
          } yield ()
      }

      def stdErr: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case isContractCall(_, Seq(arg)) =>
          F.delay(Console.err.println(prettyPrinter.buildString(arg)))
      }

      def stdErrAck: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case isContractCall(produce, Seq(arg, ack)) =>
          for {
            _ <- F.delay(Console.err.println(prettyPrinter.buildString(arg)))
            _ <- produce(Seq(Par.defaultInstance), ack)
          } yield ()
      }

      def validateRevAddress: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
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
      }

      def secp256k1Verify: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] =
        verifySignatureContract("secp256k1Verify", Secp256k1.verify)

      def ed25519Verify: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] =
        verifySignatureContract("ed25519Verify", Ed25519.verify)

      def sha256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] =
        hashContract("sha256Hash", Sha256.hash)

      def keccak256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] =
        hashContract("keccak256Hash", Keccak256.hash)

      def blake2b256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] =
        hashContract("blake2b256Hash", Blake2b256.hash)

      // TODO: rename this system process to "deployParameters"?
      def getDeployParams(
          shortLeashParams: Runtime.ShortLeashParams[F]
      ): (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case isContractCall(produce, Seq(ack)) =>
          for {
            parameters <- shortLeashParams.getParams
            _ <- parameters match {
                  case ShortLeashParameters(codeHash, phloRate, userId, timestamp) =>
                    produce(Seq(codeHash, phloRate, userId, timestamp), ack)
                  case _ =>
                    illegalArgumentException("deployParameters expects only a return channel")
                }
          } yield ()
      }

      def blockTime(
          blocktime: Runtime.BlockTime[F]
      ): (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
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
