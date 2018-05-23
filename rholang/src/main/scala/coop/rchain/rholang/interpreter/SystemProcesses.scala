package coop.rchain.rholang.interpreter

import com.google.protobuf.ByteString
import coop.rchain.crypto.encryption.Curve25519
import coop.rchain.crypto.hash.{Blake2b256, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.{IStore, produce}
import monix.eval.Task

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
        produce(store, ack, Seq(Channel(Quote(Par.defaultInstance))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
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
        produce(store, ack, Seq(Channel(Quote(Par.defaultInstance))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }

  //TODO: Consider moving it somewhere
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

  //TODO: cryptographic functions throw exceptions. Wrap it with proper effect
  //  The following methods will be made available to contract authors.
  def secp256k1Verify(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                      dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack)) =>
      //TODO: use actual secp256k1 algorithm
      Task.now(Secp256k1.verify(data, signature, pub)).flatMap { verified =>
        produce(store, ack, Seq(Channel(Quote(Par(exprs = Seq(Expr(GBool(verified))))))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }

  def ed25519Verify(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                    dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(IsByteArray(data), IsByteArray(signature), IsByteArray(pub), ack)) =>
      Task(Ed25519.verify(data, signature, pub)).flatMap { verified =>
        produce(store, ack, Seq(Channel(Quote(Par(exprs = Seq(Expr(GBool(verified))))))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }

  def curve25519Encrypt(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                        dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
  case Seq(Seq(IsByteArray(pub), IsByteArray(sec), IsByteArray(nonce), IsByteArray(message), ack)) =>
    Task(Curve25519.encrypt(pub, sec, nonce, message)).flatMap { encrypted =>
      produce(store, ack, Seq(Channel(Quote(Par(exprs = Seq(Expr(GByteArray(ByteString.copyFrom(encrypted)))))))), false) match {
        case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
        case None                           => Task.unit
      }
    }
}

  def sha256Hash(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                 dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(IsByteArray(input), ack)) =>
      Task.now(Sha256.hash(input)).flatMap { hash =>
        produce(store, ack, Seq(Channel(Quote(Par(exprs = Seq(Expr(GByteArray(ByteString.copyFrom(hash)))))))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }

  def keccak256Hash(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                    dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
    case Seq(Seq(IsByteArray(input), ack)) =>
      Task.now(Keccak256.hash(input)).flatMap { hash =>
        produce(store, ack, Seq(Channel(Quote(Par(exprs = Seq(Expr(GByteArray(ByteString.copyFrom(hash)))))))), false) match {
          case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
          case None                           => Task.unit
        }
      }
  }

  def blake2b256Hash(store: IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                     dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation])
    : Seq[Seq[Channel]] => Task[Unit] = {
  case Seq(Seq(IsByteArray(input), ack)) =>
    Task.now(Blake2b256.hash(input)).flatMap { hash =>
      produce(store, ack, Seq(Channel(Quote(Par(exprs = Seq(Expr(GByteArray(ByteString.copyFrom(hash)))))))), false) match {
        case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
        case None                           => Task.unit
      }
    }
}
}
