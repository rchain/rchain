package coop.rchain.rholang.interpreter

import cats.effect.Async
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.{Blake2b256, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.ShortLeashParams.ShortLeashParameters
import coop.rchain.rholang.interpreter.Runtime.{BlockTime, RhoISpace, ShortLeashParams}
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
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
}

object SystemProcesses {

  def apply[F[_]](
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation],
      space: RhoISpace[F]
  )(implicit F: Async[F]): SystemProcesses[F] =
    new SystemProcesses[F] {

      type ContWithMetaData = ContResult[Par, BindPattern, TaggedContinuation]

      type Channels = Seq[Result[ListParWithRandomAndPhlos]]

      private val prettyPrinter = PrettyPrinter()

      private val UNLIMITED_MATCH_PHLO = matchListPar(Cost(Integer.MAX_VALUE))

      private def foldResult(
          produceResult: Either[OutOfPhlogistonsError.type, Option[(ContWithMetaData, Channels)]]
      ): F[Unit] =
        produceResult.fold(
          _ => F.raiseError(OutOfPhlogistonsError),
          _.fold(F.unit) {
            case (cont, channels) =>
              dispatcher.dispatch(unpackCont(cont), channels.map(_.value), cont.sequenceNumber)
          }
        )

      private def illegalArgumentException(msg: String): F[Unit] =
        F.raiseError(new IllegalArgumentException(msg))

      def stdOut: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (Seq(ListParWithRandomAndPhlos(Seq(arg), _, _)), _) =>
          F.delay(Console.println(prettyPrinter.buildString(arg)))
      }

      def stdOutAck: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (Seq(ListParWithRandomAndPhlos(Seq(arg, ack), rand, _)), sequenceNumber) =>
          for {
            _ <- F.delay(Console.println(prettyPrinter.buildString(arg)))
            produceResult <- space.produce(
                              ack,
                              ListParWithRandom(Seq(Par.defaultInstance), rand),
                              persist = false,
                              sequenceNumber
                            )(UNLIMITED_MATCH_PHLO)
            _ <- foldResult(produceResult)
          } yield ()
      }

      def stdErr: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (Seq(ListParWithRandomAndPhlos(Seq(arg), _, _)), _) =>
          F.delay(Console.err.println(prettyPrinter.buildString(arg)))
      }

      def stdErrAck: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (Seq(ListParWithRandomAndPhlos(Seq(arg, ack), rand, _)), sequenceNumber) =>
          for {
            _ <- F.delay(Console.err.println(prettyPrinter.buildString(arg)))
            produceResult <- space.produce(
                              ack,
                              ListParWithRandom(Seq(Par.defaultInstance), rand),
                              persist = false,
                              sequenceNumber
                            )(UNLIMITED_MATCH_PHLO)
            _ <- foldResult(produceResult)
          } yield ()
      }

      object IsByteArray {
        import coop.rchain.models.rholang.implicits._
        def unapply(p: Par): Option[Array[Byte]] =
          p.singleExpr().collect {
            case Expr(GByteArray(bs)) => bs.toByteArray
          }
      }

      def secp256k1Verify: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (
            Seq(
              ListParWithRandomAndPhlos(
                Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack),
                rand,
                _
              )
            ),
            sequenceNumber
            ) =>
          for {
            verified <- F.fromTry(Try(Secp256k1.verify(data, signature, pub)))
            produceResult <- space
                              .produce(
                                ack,
                                ListParWithRandom(Seq(Expr(GBool(verified))), rand),
                                persist = false,
                                sequenceNumber
                              )(UNLIMITED_MATCH_PHLO)
            _ <- foldResult(produceResult)
          } yield ()
        case _ =>
          illegalArgumentException(
            "secp256k1Verify expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
          )
      }

      def ed25519Verify: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (
            Seq(
              ListParWithRandomAndPhlos(
                Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack),
                rand,
                _
              )
            ),
            sequenceNumber
            ) =>
          for {
            verified <- F.fromTry(Try(Ed25519.verify(data, signature, pub)))
            produceResult <- space
                              .produce(
                                ack,
                                ListParWithRandom(Seq(Expr(GBool(verified))), rand),
                                persist = false,
                                sequenceNumber = sequenceNumber
                              )(UNLIMITED_MATCH_PHLO)
            _ <- foldResult(produceResult)
          } yield ()
        case _ =>
          illegalArgumentException(
            "ed25519Verify expects data, signature, public key (all as byte arrays), and an acknowledgement channel"
          )
      }

      def sha256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (
            Seq(ListParWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)),
            sequenceNumber
            ) =>
          for {
            hash <- F.fromTry(Try(Sha256.hash(input)))
            produceResult <- space
                              .produce(
                                ack,
                                ListParWithRandom(
                                  Seq(Expr(GByteArray(ByteString.copyFrom(hash)))),
                                  rand
                                ),
                                persist = false,
                                sequenceNumber
                              )(UNLIMITED_MATCH_PHLO)
            _ <- foldResult(produceResult)
          } yield ()
        case _ =>
          illegalArgumentException(
            "sha256Hash expects a byte array and return channel"
          )
      }

      def keccak256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (
            Seq(ListParWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)),
            sequenceNumber
            ) =>
          for {
            hash <- F.fromTry(Try(Keccak256.hash(input)))
            produceResult <- space
                              .produce(
                                ack,
                                ListParWithRandom(
                                  Seq(Expr(GByteArray(ByteString.copyFrom(hash)))),
                                  rand
                                ),
                                persist = false,
                                sequenceNumber
                              )(UNLIMITED_MATCH_PHLO)
            _ <- foldResult(produceResult)
          } yield ()

        case _ =>
          illegalArgumentException(
            "keccak256Hash expects a byte array and return channel"
          )
      }

      def blake2b256Hash: (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (
            Seq(ListParWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)),
            sequenceNumber
            ) =>
          for {
            hash <- F.fromTry(Try(Blake2b256.hash(input)))
            produceResult <- space
                              .produce(
                                ack,
                                ListParWithRandom(
                                  Seq(Expr(GByteArray(ByteString.copyFrom(hash)))),
                                  rand
                                ),
                                persist = false,
                                sequenceNumber
                              )(UNLIMITED_MATCH_PHLO)
            _ <- foldResult(produceResult)
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
        case (Seq(ListParWithRandomAndPhlos(Seq(ack), rand, _)), sequenceNumber) =>
          for {
            parameters <- shortLeashParams.getParams
            _ <- parameters match {
                  case ShortLeashParameters(codeHash, phloRate, userId, timestamp) =>
                    for {
                      produceResult <- space.produce(
                                        ack,
                                        ListParWithRandom(
                                          Seq(codeHash, phloRate, userId, timestamp),
                                          rand
                                        ),
                                        persist = false,
                                        sequenceNumber
                                      )(UNLIMITED_MATCH_PHLO)
                      _ <- foldResult(produceResult)
                    } yield ()
                  case _ =>
                    illegalArgumentException("deployParameters expects only a return channel")
                }
          } yield ()
      }

      def blockTime(
          blocktime: Runtime.BlockTime[F]
      ): (Seq[ListParWithRandomAndPhlos], Int) => F[Unit] = {
        case (Seq(ListParWithRandomAndPhlos(Seq(ack), rand, _)), sequenceNumber) =>
          for {
            time <- blocktime.timestamp.get
            produceResult <- space.produce(
                              ack,
                              ListParWithRandom(Seq(time), rand),
                              persist = false,
                              sequenceNumber
                            )(UNLIMITED_MATCH_PHLO)
            _ <- foldResult(produceResult)
          } yield ()
        case _ =>
          illegalArgumentException("blockTime expects only a return channel")
      }
    }
}
