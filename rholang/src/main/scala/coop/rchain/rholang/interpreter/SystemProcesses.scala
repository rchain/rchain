package coop.rchain.rholang.interpreter

import cats.Id
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.{Blake2b256, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray}
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits.matchListQuote
import monix.eval.Task

import scala.util.Try

object SystemProcesses {

  private val prettyPrinter = PrettyPrinter()

  private val MATCH_UNLIMITED_PHLOS = matchListQuote(Cost(Integer.MAX_VALUE))

  def stdout: Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case (Seq(ListChannelWithRandomAndPhlos(Seq(arg), _, _))) =>
      Task.now(Console.println(prettyPrinter.buildString(arg)))
  }

  private implicit class ProduceOps(
      res: Id[
        Either[OutOfPhlogistonsError.type, Option[
          (TaggedContinuation, Seq[ListChannelWithRandomAndPhlos])
        ]]
      ]
  ) {
    def foldResult(
        dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
    ): Task[Unit] =
      res.fold(err => Task.raiseError(OutOfPhlogistonsError), _.fold(Task.unit) {
        case (cont, channels) => _dispatch(dispatcher)(cont, channels)
      })
  }

  def stdoutAck(
      space: RhoISpace,
      dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case Seq(ListChannelWithRandomAndPhlos(Seq(arg, ack), rand, cost)) =>
      Task.now(Console.println(prettyPrinter.buildString(arg))).flatMap { (_: Unit) =>
        space
          .produce(
            ack,
            ListChannelWithRandom(Seq(Channel(Quote(Par.defaultInstance))), rand),
            false
          )(MATCH_UNLIMITED_PHLOS)
          .foldResult(dispatcher)
      }
  }

  def stderr: Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case (Seq(ListChannelWithRandomAndPhlos(Seq(arg), _, _))) =>
      Task.now(Console.err.println(prettyPrinter.buildString(arg)))
  }

  def stderrAck(
      space: RhoISpace,
      dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case Seq(ListChannelWithRandomAndPhlos(Seq(arg, ack), rand, _)) =>
      Task.now(Console.err.println(prettyPrinter.buildString(arg))).flatMap { (_: Unit) =>
        space
          .produce(
            ack,
            ListChannelWithRandom(Seq(Channel(Quote(Par.defaultInstance))), rand),
            false
          )(MATCH_UNLIMITED_PHLOS)
          .foldResult(dispatcher)
      }
  }

  object IsByteArray {
    import coop.rchain.models.rholang.implicits._
    def unapply(p: Channel): Option[Array[Byte]] =
      p match {
        case Channel(Quote(par)) =>
          par.singleExpr().collect {
            case Expr(GByteArray(bs)) => bs.toByteArray
          }
        case _ => None
      }
  }

  //  The following methods will be made available to contract authors.

  def secp256k1Verify(
      space: RhoISpace,
      dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case Seq(
        ListChannelWithRandomAndPhlos(
          Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack),
          rand,
          _
        )
        ) =>
      Task.fromTry(Try(Secp256k1.verify(data, signature, pub))).flatMap { verified =>
        space
          .produce(
            ack,
            ListChannelWithRandom(Seq(Channel(Quote(Expr(GBool(verified))))), rand),
            false
          )(MATCH_UNLIMITED_PHLOS)
          .foldResult(dispatcher)
      }
  }

  def ed25519Verify(
      space: RhoISpace,
      dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case Seq(
        ListChannelWithRandomAndPhlos(
          Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack),
          rand,
          _
        )
        ) =>
      Task.fromTry(Try(Ed25519.verify(data, signature, pub))).flatMap { verified =>
        space
          .produce(
            ack,
            ListChannelWithRandom(Seq(Channel(Quote(Expr(GBool(verified))))), rand),
            false
          )(MATCH_UNLIMITED_PHLOS)
          .foldResult(dispatcher)
      }
    case _ =>
      illegalArgumentException(
        "ed25519Verify expects data, signature and public key (all as byte arrays) and ack channel as arguments"
      )
  }

  def sha256Hash(
      space: RhoISpace,
      dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case Seq(ListChannelWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)) =>
      Task.fromTry(Try(Sha256.hash(input))).flatMap { hash =>
        space
          .produce(
            ack,
            ListChannelWithRandom(
              Seq(Channel(Quote(Expr(GByteArray(ByteString.copyFrom(hash)))))),
              rand
            ),
            false
          )(MATCH_UNLIMITED_PHLOS)
          .foldResult(dispatcher)
      }
    case _ =>
      illegalArgumentException("sha256Hash expects byte array and return channel as arguments")
  }

  def keccak256Hash(
      space: RhoISpace,
      dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case Seq(ListChannelWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)) =>
      Task.fromTry(Try(Keccak256.hash(input))).flatMap { hash =>
        space
          .produce(
            ack,
            ListChannelWithRandom(
              Seq(Channel(Quote(Expr(GByteArray(ByteString.copyFrom(hash)))))),
              rand
            ),
            false
          )(MATCH_UNLIMITED_PHLOS)
          .foldResult(dispatcher)
      }
    case _ =>
      illegalArgumentException("keccak256Hash expects byte array and return channel as arguments")
  }

  def blake2b256Hash(
      space: RhoISpace,
      dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
  ): Seq[ListChannelWithRandomAndPhlos] => Task[Unit] = {
    case Seq(ListChannelWithRandomAndPhlos(Seq(IsByteArray(input), ack), rand, _)) =>
      Task.fromTry(Try(Blake2b256.hash(input))).flatMap { hash =>
        space
          .produce(
            ack,
            ListChannelWithRandom(
              Seq(Channel(Quote(Expr(GByteArray(ByteString.copyFrom(hash)))))),
              rand
            ),
            false
          )(MATCH_UNLIMITED_PHLOS)
          .foldResult(dispatcher)
      }
    case _ =>
      illegalArgumentException("blake2b256Hash expects byte array and return channel as arguments")
  }

  private def _dispatch(
      dispatcher: Dispatch[Task, ListChannelWithRandomAndPhlos, TaggedContinuation]
  )(cont: TaggedContinuation, dataList: Seq[ListChannelWithRandomAndPhlos]): Task[Unit] =
    dispatcher.dispatch(cont, dataList)

  private def illegalArgumentException(msg: String): Task[Unit] =
    Task.raiseError(new IllegalArgumentException(msg))
}
