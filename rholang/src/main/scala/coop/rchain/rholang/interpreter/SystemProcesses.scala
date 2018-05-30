package coop.rchain.rholang.interpreter

import com.google.protobuf.ByteString
import coop.rchain.crypto.encryption.Curve25519
import coop.rchain.crypto.hash.{Blake2b256, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.{produce, IStore}
import monix.eval.Task
import implicits._

import scala.util.Try

object SystemProcesses {

  private val prettyPrinter = PrettyPrinter()

  def stdout: Seq[Seq[Channel]] => Task[Unit] = {
    case (Seq(Seq(arg))) =>
      Task(Console.println(prettyPrinter.buildString(arg)))
  }

  def stdoutAck(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(arg, ack)) =>
      Task(Console.println(prettyPrinter.buildString(arg))).flatMap { (_: Unit) =>
        produce(store, ack, Seq(Channel(Quote(Par.defaultInstance))), false)
          .fold(Task.unit) { case (cont, channels) => _dispatch(dispatcher)(cont, channels) }
      }
  }

  def stderr: Seq[Seq[Channel]] => Task[Unit] = {
    case (Seq(Seq(arg))) =>
      Task(Console.err.println(prettyPrinter.buildString(arg)))
  }

  def stderrAck(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(arg, ack)) =>
      Task(Console.err.println(prettyPrinter.buildString(arg))).flatMap { (_: Unit) =>
        produce(store, ack, Seq(Channel(Quote(Par.defaultInstance))), false)
          .fold(Task.unit) { case (cont, channels) => _dispatch(dispatcher)(cont, channels) }
      }
  }

  object IsByteArray {
    import implicits._
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

  //TODO(mateusz.gorski): we decided to look into delivering secp256k1 library (https://github.com/bitcoin-core/secp256k1)
  // as separate jar in the future
//  def secp256k1Verify(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
//                      dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
//    : Seq[Seq[Channel]] => Task[Unit] = {
//    case Seq(Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack)) =>
//      Task.fromTry(Try(Secp256k1.verify(data, signature, pub))).flatMap { verified =>
//        produce(store, ack, Seq(Channel(Quote(Expr(GBool(verified))))), false)
//          .fold(Task.unit) { case (cont, channels) => _dispatch(dispatcher)(cont, channels) }
//      }
//  }

  def ed25519Verify(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                    dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack)) =>
      Task.fromTry(Try(Ed25519.verify(data, signature, pub))).flatMap { verified =>
        produce(store, ack, Seq(Channel(Quote(Expr(GBool(verified))))), false)
          .fold(Task.unit) { case (cont, channels) => _dispatch(dispatcher)(cont, channels) }
      }
    case _ =>
      illegalArgumentException(
        "ed25519Verify expects data, signature and public key (all as byte arrays) and ack channel as arguments")
  }

  def curve25519Encrypt(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                        dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(
        Seq(IsByteArray(pub), IsByteArray(sec), IsByteArray(nonce), IsByteArray(message), ack)) =>
      Task.fromTry(Try(Curve25519.encrypt(pub, sec, nonce, message))).flatMap { encrypted =>
        produce(store,
                ack,
                Seq(Channel(Quote(Expr(GByteArray(ByteString.copyFrom(encrypted)))))),
                false)
          .fold(Task.unit) { case (cont, channels) => _dispatch(dispatcher)(cont, channels) }
      }
    case _ =>
      illegalArgumentException(
        "curve25519Encrypt expects public key, private key, nonce, message (all as byte arrays) and ack channel as arguments")
  }

  def sha256Hash(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                 dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(IsByteArray(input), ack)) =>
      Task.fromTry(Try(Sha256.hash(input))).flatMap { hash =>
        produce(store, ack, Seq(Channel(Quote(Expr(GByteArray(ByteString.copyFrom(hash)))))), false)
          .fold(Task.unit) { case (cont, channels) => _dispatch(dispatcher)(cont, channels) }
      }
    case _ =>
      illegalArgumentException("sha256Hash expects byte array and return channel as arguments")
  }

  def keccak256Hash(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                    dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(IsByteArray(input), ack)) =>
      Task.fromTry(Try(Keccak256.hash(input))).flatMap { hash =>
        produce(store, ack, Seq(Channel(Quote(Expr(GByteArray(ByteString.copyFrom(hash)))))), false)
          .fold(Task.unit) { case (cont, channels) => _dispatch(dispatcher)(cont, channels) }
      }
    case _ =>
      illegalArgumentException("keccak256Hash expects byte array and return channel as arguments")
  }

  def blake2b256Hash(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                     dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(IsByteArray(input), ack)) =>
      Task.fromTry(Try(Blake2b256.hash(input))).flatMap { hash =>
        produce(store, ack, Seq(Channel(Quote(Expr(GByteArray(ByteString.copyFrom(hash)))))), false)
          .fold(Task.unit) { case (cont, channels) => _dispatch(dispatcher)(cont, channels) }
      }
    case _ =>
      illegalArgumentException("blake2b256Hash expects byte array and return channel as arguments")
  }

  private def _dispatch(dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])(
      cont: TaggedContinuation,
      dataList: Seq[Seq[Channel]]): Task[Unit] =
    dispatcher.dispatch(cont, dataList)

  private def illegalArgumentException(msg: String): Task[Unit] =
    Task.raiseError(new IllegalArgumentException(msg))
}
