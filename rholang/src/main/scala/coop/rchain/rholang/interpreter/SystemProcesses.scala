package coop.rchain.rholang.interpreter

import cats.implicits._
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.{Blake2b256, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rspace.util._
import coop.rchain.rspace.{ContResult, Result}
import scala.collection.immutable
import scala.util.Try

object SystemProcesses {

  private val prettyPrinter = PrettyPrinter()

  private val MATCH_UNLIMITED_PHLOS = matchListPar(Cost(Integer.MAX_VALUE))

  def stdout[F[_]: Sync]: Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(arg), _, _)) =>
      Sync[F].delay(Console.println(prettyPrinter.buildString(arg)))
  }

  private type ContinuationWithMetadata = ContResult[Par, BindPattern, TaggedContinuation]
  private type SuccessfulResult =
    Option[(ContinuationWithMetadata, immutable.Seq[Result[ListParWithRandomAndPhlos]])]
  private type ProduceResult = Either[errors.OutOfPhlogistonsError.type, SuccessfulResult]

  private implicit class ProduceOps[F[_]: Sync](res: F[ProduceResult]) {
    def foldResult(
        dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
    ): F[Unit] = {
      def handleSuccess(success: SuccessfulResult): F[Unit] =
        success.fold(Sync[F].pure(())) {
          case (cont, channels) =>
            dispatcher.dispatch(unpackCont(cont), channels.map(_.value))
        }

      res.flatMap(_.fold(_ => Sync[F].raiseError(OutOfPhlogistonsError), handleSuccess))
    }
  }

  def stdoutAck[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(arg, ack), rand, _)) =>
      for {
        _ <- Sync[F].delay(Console.println(prettyPrinter.buildString(arg)))
        produced <- space
                     .produce(
                       ack,
                       ListParWithRandom(Seq(Par.defaultInstance), rand),
                       persist = false
                     )(MATCH_UNLIMITED_PHLOS)
                     .foldResult(dispatcher)
      } yield produced

  }

  def stderr[F[_]: Sync]: Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(arg), _, _)) =>
      Sync[F].delay(Console.err.println(prettyPrinter.buildString(arg)))
  }

  def stderrAck[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(arg, ack), rand, _)) =>
      for {
        _ <- Sync[F].delay(Console.err.println(prettyPrinter.buildString(arg)))
        produced <- space
                     .produce(
                       ack,
                       ListParWithRandom(Seq(Par.defaultInstance), rand),
                       persist = false
                     )(MATCH_UNLIMITED_PHLOS)
                     .foldResult(dispatcher)
      } yield produced

  }

  object IsByteArray {
    import coop.rchain.models.rholang.implicits._
    def unapply(p: Par): Option[Array[Byte]] =
      p.singleExpr().collect {
        case Expr(GByteArray(bs)) => bs.toByteArray
      }
  }

  //  The following methods will be made available to contract authors.

  def secp256k1Verify[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(
        ListParWithRandomAndPhlos(
          Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack),
          rand,
          _
        )
        ) =>
      for {
        verified <- Sync[F].fromTry(Try(Secp256k1.verify(data, signature, pub)))
        produced <- space
                     .produce(
                       ack,
                       ListParWithRandom(Seq(Expr(GBool(verified))), rand),
                       persist = false
                     )(MATCH_UNLIMITED_PHLOS)
                     .foldResult(dispatcher)
      } yield produced

  }

  def ed25519Verify[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(
        ListParWithRandomAndPhlos(
          Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack),
          rand,
          _
        )
        ) =>
      for {
        verified <- Sync[F].fromTry(Try(Ed25519.verify(data, signature, pub)))
        produced <- space
                     .produce(
                       ack,
                       ListParWithRandom(Seq(Expr(GBool(verified))), rand),
                       persist = false
                     )(MATCH_UNLIMITED_PHLOS)
                     .foldResult(dispatcher)
      } yield produced
    case _ =>
      illegalArgumentException(
        "ed25519Verify expects data, signature and public key (all as byte arrays) and ack channel as arguments"
      )
  }

  def sha256Hash[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)) =>
      for {
        hash <- Sync[F].fromTry(Try(Sha256.hash(input)))
        produced <- space
                     .produce(
                       ack,
                       ListParWithRandom(
                         Seq(Expr(GByteArray(ByteString.copyFrom(hash)))),
                         rand
                       ),
                       persist = false
                     )(MATCH_UNLIMITED_PHLOS)
                     .foldResult(dispatcher)
      } yield produced
    case _ =>
      illegalArgumentException("sha256Hash expects byte array and return channel as arguments")
  }

  def keccak256Hash[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)) =>
      for {
        hash <- Sync[F].fromTry(Try(Keccak256.hash(input)))
        produced <- space
                     .produce(
                       ack,
                       ListParWithRandom(
                         Seq(Expr(GByteArray(ByteString.copyFrom(hash)))),
                         rand
                       ),
                       persist = false
                     )(MATCH_UNLIMITED_PHLOS)
                     .foldResult(dispatcher)
      } yield produced

    case _ =>
      illegalArgumentException("keccak256Hash expects byte array and return channel as arguments")
  }

  def blake2b256Hash[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)) =>
      for {
        hash <- Sync[F].fromTry(Try(Blake2b256.hash(input)))
        produced <- space
                     .produce(
                       ack,
                       ListParWithRandom(
                         Seq(Expr(GByteArray(ByteString.copyFrom(hash)))),
                         rand
                       ),
                       persist = false
                     )(MATCH_UNLIMITED_PHLOS)
                     .foldResult(dispatcher)
      } yield produced
    case _ =>
      illegalArgumentException("blake2b256Hash expects byte array and return channel as arguments")
  }

  def getDeployParams[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation],
      shortLeashParams: Runtime.ShortLeashParams[F]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(ack), rand, _)) =>
      shortLeashParams.getParams.flatMap { parameters =>
        import parameters._
        space
          .produce(
            ack,
            ListParWithRandom(Seq(codeHash, phloRate, userId, timestamp), rand),
            persist = false
          )(MATCH_UNLIMITED_PHLOS)
          .foldResult(dispatcher)
      }
    case _ =>
      illegalArgumentException("getDeployParams expects only a return channel.")
  }

  def blockTime[F[_]: Sync](
      space: RhoISpace[F],
      dispatcher: Dispatch[F, ListParWithRandomAndPhlos, TaggedContinuation],
      blockTime: Runtime.BlockTime[F]
  ): Seq[ListParWithRandomAndPhlos] => F[Unit] = {
    case Seq(ListParWithRandomAndPhlos(Seq(ack), rand, _)) =>
      for {
        timestamp <- blockTime.timestamp.get
        produced <- space
                     .produce(
                       ack,
                       ListParWithRandom(Seq(timestamp), rand),
                       persist = false
                     )(MATCH_UNLIMITED_PHLOS)
                     .foldResult(dispatcher)
      } yield produced
    case _ =>
      illegalArgumentException("blockTime expects only a return channel.")
  }

  private def illegalArgumentException[F[_]: Sync](msg: String): F[Unit] =
    Sync[F].raiseError(new IllegalArgumentException(msg))
}
