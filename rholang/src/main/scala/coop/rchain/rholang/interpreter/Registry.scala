package coop.rchain.rholang.interpreter

import com.google.protobuf.ByteString
import com.google.protobuf.ByteString.ByteIterator
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.TaggedContinuation.TaggedCont.ScalaBodyRef
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.{ISpace, IStore}
import monix.eval.Task
import scala.collection.immutable.Seq
import scala.collection.{Seq => RootSeq}

class Registry(private val space: ISpace[Channel,
                                         BindPattern,
                                         ListChannelWithRandom,
                                         ListChannelWithRandom,
                                         TaggedContinuation],
               private val dispatcher: Dispatch[Task, ListChannelWithRandom, TaggedContinuation]) {
  import Registry._
  def commonPrefix(b1: ByteString, b2: ByteString): ByteString = {
    val prefixOut = ByteString.newOutput()
    def loop(it1: ByteIterator, it2: ByteIterator): ByteString = {
      val b: Byte = it1.nextByte
      if (it2.nextByte == b) {
        prefixOut.write(b.toInt)
        loop(it1, it2)
      } else {
        prefixOut.toByteString()
      }
    }
    loop(b1.iterator, b2.iterator)
  }

  val lookupRef: Long = 10L
  val lookupPatterns = List(
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)), ChanVar(FreeVar(1))),
      freeCount = 2))
  val lookupChannels = List(Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](10))))))

  // This is the only construction side effect.
  space.install(lookupChannels, lookupPatterns, TaggedContinuation(ScalaBodyRef(lookupRef)))

  val lookupCallbackRef: Long = 11L
  val prefixRetReplacePattern = BindPattern(
    Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
        ChanVar(FreeVar(1)),
        ChanVar(FreeVar(2))),
    freeCount = 3)
  val triePattern = BindPattern(
    Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true))),
    freeCount = 1)

  def handleResult(result: Option[(TaggedContinuation, Seq[ListChannelWithRandom])]): Task[Unit] =
    result match {
      case Some((continuation, dataList)) => dispatcher.dispatch(continuation, dataList)
      case None                           => Task.unit
    }

  def succeed(ret: Channel, result: Par, rand: Blake2b512Random): Task[Unit] =
    ret match {
      case Channel(q @ Quote(_)) =>
        handleResult(space.produce(q, ListChannelWithRandom(Seq(Quote(result)), rand, None), false))
      case _ => Task.unit
    }

  def fail(ret: Channel, rand: Blake2b512Random): Task[Unit] = succeed(ret, Par(), rand)

  def replace(data: Channel, replaceChan: Channel, dataRand: Blake2b512Random): Task[Unit] =
    replaceChan match {
      case Channel(q @ Quote(_)) =>
        handleResult(
          space
            .produce(q, ListChannelWithRandom(Seq(data), dataRand, None), false))
      case _ => Task.unit
    }

  def failAndReplace(data: Channel,
                     replaceChan: Channel,
                     retChan: Channel,
                     dataRand: Blake2b512Random,
                     failRand: Blake2b512Random): Task[Unit] =
    for {
      _ <- replace(data, replaceChan, dataRand)
      _ <- fail(retChan, failRand)
    } yield ()

  def fetchData(dataSource: Quote,
                key: Channel,
                ret: Channel,
                rand: Blake2b512Random): Task[Unit] = {
    val channel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(Quote(channel),
                          ListChannelWithRandom(Seq(key, ret, Channel(dataSource)), rand, None),
                          false))
      _ <- handleResult(
            space.consume(
              Seq[Channel](Quote(channel), dataSource),
              Seq(prefixRetReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(lookupCallbackRef)),
              false
            ))
    } yield ()
  }

  def lookup(args: RootSeq[ListChannelWithRandom]): Task[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, ret), rand, cost)) =>
        key match {
          case Channel(Quote(keyPar)) =>
            keyPar.singleExpr match {
              case Some(Expr(GByteArray(_))) =>
                fetchData(Quote(registryRoot), key, ret, rand)
              case _ =>
                fail(ret, rand)
            }
          case _ =>
            fail(ret, rand)
        }
      case _ => Task.unit
    }

  // A lookup result should result in 1 of 3 things
  // Result not there. Return Nil
  // Result there, return it.
  // Further lookup needed, recurse.

  def lookupCallback(args: RootSeq[ListChannelWithRandom]): Task[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, ret, replaceChan), callRand, callCost),
               ListChannelWithRandom(Seq(data), dataRand, dataCost)) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        key match {
          case Channel(Quote(keyPar)) =>
            keyPar.singleExpr match {
              case Some(Expr(GByteArray(bs))) =>
                val head = if (bs.isEmpty()) bs else bs.substring(0, 1)
                val tail = if (bs.isEmpty()) bs else bs.substring(1)
                data match {
                  case Channel(Quote(dataPar)) =>
                    dataPar.singleExpr match {
                      case Some(Expr(EMapBody(parMap))) =>
                        parMap.ps.get(Expr(GByteArray(head))) match {
                          case None => localFail()
                          case Some(value) =>
                            value.singleExpr() match {
                              case Some(Expr(ETupleBody(ETuple(ps, _, _)))) =>
                                if (ps.length != 3)
                                  localFail()
                                else
                                  // The second tuple field should be a bytearray in both cases.
                                  ps(1).singleExpr() match {
                                    case Some(Expr(GByteArray(edgeAdditional))) =>
                                      ps(0).singleExpr() match {
                                        case Some(Expr(GInt(0))) =>
                                          if (tail == edgeAdditional)
                                            replace(data, replaceChan, dataRand).flatMap(_ =>
                                              succeed(ret, ps(2), callRand))
                                          else
                                            localFail()
                                        case Some(Expr(GInt(1))) =>
                                          if (tail.startsWith(edgeAdditional)) {
                                            val newKey = tail.substring(edgeAdditional.size)

                                            replace(data, replaceChan, dataRand).flatMap(
                                              _ =>
                                                fetchData(Quote(ps(2)),
                                                          Channel(Quote(GByteArray(newKey))),
                                                          ret,
                                                          callRand))
                                          } else
                                            localFail()
                                        case _ => localFail()
                                      }
                                    case _ => localFail()
                                  }
                              case _ => localFail()
                            }
                        }
                      case _ => localFail()
                    }
                  case _ => localFail()
                }
              case _ =>
                localFail()
            }
          case _ =>
            localFail()
        }
      case _ => Task.unit
    }

  val testingDispatchTable: Map[Long, Function1[RootSeq[ListChannelWithRandom], Task[Unit]]] =
    Map(lookupRef -> lookup, lookupCallbackRef -> lookupCallback)
}

object Registry {
  val registryRoot = GPrivate(
    ByteString.copyFrom(
      Base16.decode("a4fd447dedfc960485983ee817632cf36d79f45fd1796019edfb4a84a81d1697")))

  def testTableCreator(space: Runtime.RhoISpace, dispatch: Runtime.RhoDispatch) = {
    val reg = new Registry(space, dispatch)
    reg.testingDispatchTable
  }
}
