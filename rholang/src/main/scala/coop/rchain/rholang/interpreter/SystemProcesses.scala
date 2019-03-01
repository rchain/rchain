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
  }

  object String {
    def unapply(p: Par): Option[String] =
      p.singleExpr().collect {
        case Expr(GString(bs)) => bs
      }
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

      type Producer = (Seq[Par], Par) => F[Unit]

      private val prettyPrinter = PrettyPrinter()

      private val UNLIMITED_MATCH_PHLO = matchListPar(Cost(Integer.MAX_VALUE))

      object ContractCall {
        private def produce(rand : Blake2b512Random, sequenceNumber : Int)
                           (values: Seq[Par], ch: Par): F[Unit] =
          for {
            produceResult <- space.produce(
              ch,
              ListParWithRandom(values, rand),
              persist = false,
              sequenceNumber
            )(UNLIMITED_MATCH_PHLO)
            _ <- produceResult.fold(
              _ => F.raiseError(OutOfPhlogistonsError),
              _.fold(F.unit) {
                case (cont, channels) =>
                  dispatcher.dispatch(unpackCont(cont), channels.map(_.value), cont.sequenceNumber)
              }
            )
          } yield ()

        def unapply(contractArgs: (Seq[ListParWithRandomAndPhlos], Int)): Option[
          (Producer, Seq[Par])
        ] =
          contractArgs match {
            case (
                Seq(
                  ListParWithRandomAndPhlos(
                    args,
                    rand,
                    _
                  )
                ),
                sequenceNumber
                ) =>
              Some((produce(rand, sequenceNumber), args))
            case _ => None
          }
      }

      private def illegalArgumentException(msg: String): F[Unit] =
        F.raiseError(new IllegalArgumentException(msg))

      def stdOut: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(_, Seq(arg)) =>
          F.delay(Console.println(prettyPrinter.buildString(arg)))
      }

      def stdOutAck: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(arg, ack)
            ) =>
          for {
            _ <- F.delay(Console.println(prettyPrinter.buildString(arg)))
            _ <- produce(Seq(Par.defaultInstance), ack)
          } yield ()
      }

      def stdErr: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(_, Seq(arg)) =>
          F.delay(Console.err.println(prettyPrinter.buildString(arg)))
      }

      def stdErrAck: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(arg, ack)
            ) =>
          for {
            _ <- F.delay(Console.err.println(prettyPrinter.buildString(arg)))
            _ <- produce(Seq(Par.defaultInstance), ack)
          } yield ()
      }

      def getErrorMessagePar(str: String): Par =
        RevAddress
          .parse(str)
          .swap
          .toOption
          .fold(Par())(GString(_))

      def validateRevAddress: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(RhoType.String("validate"), RhoType.String(address), ack)
            ) =>
          for {
            result <- F.delay(getErrorMessagePar(address))
            _      <- produce(Seq(result), ack)
          } yield ()
      }

      def secp256k1Verify: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(
              RhoType.ByteArray(data),
              RhoType.ByteArray(signature),
              RhoType.ByteArray(pub),
              ack
            )
            ) =>
          for {
            verified <- F.fromTry(Try(Secp256k1.verify(data, signature, pub)))
            _        <- produce(Seq(Expr(GBool(verified))), ack)
          } yield ()
        case _ =>
          illegalArgumentException(
            "secp256k1Verify expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
          )
      }

      def ed25519Verify: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(
              RhoType.ByteArray(data),
              RhoType.ByteArray(signature),
              RhoType.ByteArray(pub),
              ack
            )
            ) =>
          for {
            verified <- F.fromTry(Try(Ed25519.verify(data, signature, pub)))
            _        <- produce(Seq(Expr(GBool(verified))), ack)
          } yield ()
        case _ =>
          illegalArgumentException(
            "ed25519Verify expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
          )
      }

      def sha256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(RhoType.ByteArray(input), ack)
            ) =>
          for {
            hash <- F.fromTry(Try(Sha256.hash(input)))
            _    <- produce(Seq(Expr(GByteArray(ByteString.copyFrom(hash)))), ack)
          } yield ()
        case _ =>
          illegalArgumentException(
            "sha256Hash expects a byte array and return channel"
          )
      }

      def keccak256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(RhoType.ByteArray(input), ack)
            ) =>
          for {
            hash <- F.fromTry(Try(Keccak256.hash(input)))
            _    <- produce(Seq(Expr(GByteArray(ByteString.copyFrom(hash)))), ack)
          } yield ()

        case _ =>
          illegalArgumentException(
            "keccak256Hash expects a byte array and return channel"
          )
      }

      def blake2b256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(RhoType.ByteArray(input), ack)
            ) =>
          for {
            hash <- F.fromTry(Try(Blake2b256.hash(input)))
            _    <- produce(Seq(Expr(GByteArray(ByteString.copyFrom(hash)))), ack)
          } yield ()
        case _ =>
          illegalArgumentException(
            "blake2b256Hash expects a byte array and return channel"
          )
      }

      // TODO: rename this system process to "deployParameters"?
      def getDeployParams(
          shortLeashParams: Runtime.ShortLeashParams[F]
      ): (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case ContractCall(
            produce,
            Seq(ack)
            ) =>
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
        case ContractCall(
            produce,
            Seq(ack)
            ) =>
          for {
            time <- blocktime.timestamp.get
            _    <- produce(Seq(time), ack)
          } yield ()
        case _ =>
          illegalArgumentException("blockTime expects only a return channel")
      }
    }
}
