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
    def loop(it1: ByteIterator, it2: ByteIterator): ByteString =
      if (!it1.hasNext || !it2.hasNext) {
        prefixOut.toByteString
      } else {
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

  def safeUncons(b: ByteString): (ByteString, ByteString) = {
    val head = if (b.isEmpty()) b else b.substring(0, 1)
    val tail = if (b.isEmpty()) b else b.substring(1)
    (head, tail)
  }

  val lookupRef: Long = 10L
  val lookupPatterns = List(
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)), ChanVar(FreeVar(1))),
      freeCount = 2))
  val lookupChannels  = List(Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](10))))))
  val insertRef: Long = 12L
  val insertPatterns = List(
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
          Quote(Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)),
          ChanVar(FreeVar(2))),
      freeCount = 3
    ))
  val insertChannels = List(Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](12))))))
  val deletePatterns = List(
    BindPattern(
      Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)), ChanVar(FreeVar(1))),
      freeCount = 2))
  val deleteChannels = List(Channel(Quote(GPrivate(ByteString.copyFrom(Array[Byte](14))))))

  def testInstall(): Unit = {
    space.install(lookupChannels, lookupPatterns, TaggedContinuation(ScalaBodyRef(lookupRef)))
    space.install(insertChannels, insertPatterns, TaggedContinuation(ScalaBodyRef(insertRef)))
    space.install(deleteChannels, deletePatterns, TaggedContinuation(ScalaBodyRef(deleteRef)))
  }

  val lookupCallbackRef: Long = 11L
  val prefixRetReplacePattern = BindPattern(
    Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
        ChanVar(FreeVar(1)),
        ChanVar(FreeVar(2))),
    freeCount = 3)
  val prefixValueRetReplacePattern = BindPattern(
    Seq(
      Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
      Quote(Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)),
      ChanVar(FreeVar(2)),
      ChanVar(FreeVar(3))
    ),
    freeCount = 4
  )
  val parentKeyDataReplacePattern = BindPattern(
    Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true)),
        Quote(Par(exprs = Seq(EVar(FreeVar(1))), connectiveUsed = true)),
        ChanVar(FreeVar(2))),
    freeCount = 3
  )
  val triePattern = BindPattern(
    Seq(Quote(Par(exprs = Seq(EVar(FreeVar(0))), connectiveUsed = true))),
    freeCount = 1)

  val insertCallbackRef: Long = 13L

  val deleteRef: Long             = 14L
  val deleteRootCallbackRef: Long = 15L
  val deleteCallbackRef: Long     = 16L

  def parByteArray(bs: ByteString): Par = GByteArray(bs)

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

  def fetchDataLookup(dataSource: Quote,
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

  def fetchDataInsert(dataSource: Quote,
                      key: Channel,
                      value: Channel,
                      ret: Channel,
                      rand: Blake2b512Random): Task[Unit] = {
    val channel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(
              Quote(channel),
              ListChannelWithRandom(Seq(key, value, ret, Channel(dataSource)), rand, None),
              false))
      _ <- handleResult(
            space.consume(
              Seq[Channel](Quote(channel), dataSource),
              Seq(prefixValueRetReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(insertCallbackRef)),
              false
            ))
    } yield ()
  }

  def fetchDataRootDelete(dataSource: Quote,
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
              TaggedContinuation(ScalaBodyRef(deleteRootCallbackRef)),
              false
            ))
    } yield ()
  }

  def fetchDataDelete(dataSource: Quote,
                      key: Channel,
                      ret: Channel,
                      rand: Blake2b512Random,
                      parentKey: Channel,
                      parentData: Channel,
                      parentReplace: Channel,
                      parentRand: Blake2b512Random): Task[Unit] = {
    val keyChannel: Par    = GPrivate(ByteString.copyFrom(rand.next()))
    val parentChannel: Par = GPrivate(ByteString.copyFrom(rand.next()))
    for {
      _ <- handleResult(
            space.produce(Quote(keyChannel),
                          ListChannelWithRandom(Seq(key, ret, Channel(dataSource)), rand, None),
                          false))
      _ <- handleResult(
            space.produce(
              Quote(parentChannel),
              ListChannelWithRandom(Seq(parentKey, parentData, parentReplace), parentRand, None),
              false))
      _ <- handleResult(
            space.consume(
              Seq[Channel](Quote(keyChannel), Quote(parentChannel), dataSource),
              Seq(prefixRetReplacePattern, parentKeyDataReplacePattern, triePattern),
              TaggedContinuation(ScalaBodyRef(deleteCallbackRef)),
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
                fetchDataLookup(Quote(registryRoot), key, ret, rand)
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
                val (head, tail) = safeUncons(bs)
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
                                                fetchDataLookup(
                                                  Quote(ps(2)),
                                                  Channel(Quote(parByteArray(newKey))),
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
              case _ => localFail()
            }
          case _ => localFail()
        }
      case _ => Task.unit
    }

  def insert(args: RootSeq[ListChannelWithRandom]): Task[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, value, ret), rand, cost)) =>
        key match {
          case Channel(Quote(keyPar)) =>
            keyPar.singleExpr match {
              case Some(Expr(GByteArray(_))) =>
                fetchDataInsert(Quote(registryRoot), key, value, ret, rand)
              case _ =>
                fail(ret, rand)
            }
          case _ =>
            fail(ret, rand)
        }
      case _ => Task.unit
    }

  def insertCallback(args: RootSeq[ListChannelWithRandom]): Task[Unit] = {
    args match {
      case Seq(ListChannelWithRandom(Seq(key, value, ret, replaceChan), callRand, callCost),
               ListChannelWithRandom(Seq(data), dataRand, dataCost)) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        key match {
          case Channel(Quote(keyPar)) =>
            keyPar.singleExpr match {
              case Some(Expr(GByteArray(bs))) =>
                val (head, tail) = safeUncons(bs)
                data match {
                  case Channel(Quote(dataPar)) =>
                    dataPar.singleExpr match {
                      case Some(Expr(EMapBody(parMap))) => {
                        def insert() =
                          value match {
                            case Channel(Quote(valuePar)) =>
                              val tuple: Par = ETuple(Seq(GInt(0), parByteArray(tail), valuePar))
                              val newMap: Par = ParMap(
                                SortedParMap(parMap.ps + (parByteArray(head) -> tuple)))
                              replace(Channel(Quote(newMap)), replaceChan, dataRand).flatMap(_ =>
                                succeed(ret, valuePar, callRand))
                            case _ => localFail()
                          }
                        parMap.ps.get(parByteArray(head)) match {
                          case None => insert()
                          case Some(mapEntry) =>
                            mapEntry.singleExpr() match {
                              case Some(Expr(ETupleBody(ETuple(ps, _, _)))) =>
                                if (ps.length != 3)
                                  localFail()
                                else {
                                  // The second tuple field should be a bytearray in both cases.
                                  ps(1).singleExpr() match {
                                    case Some(Expr(GByteArray(edgeAdditional))) =>
                                      def split() =
                                        value match {
                                          case Channel(Quote(valuePar)) =>
                                            val outgoingEdgeStr: ByteString =
                                              commonPrefix(edgeAdditional, tail)
                                            val outgoingEdge: Par = parByteArray(outgoingEdgeStr)
                                            val oldEdgeStr: ByteString =
                                              edgeAdditional.substring(outgoingEdgeStr.size())
                                            val newEdgeStr: ByteString =
                                              tail.substring(outgoingEdgeStr.size())
                                            val (oldEdgeHead, oldEdgeTail) = safeUncons(oldEdgeStr)
                                            val (newEdgeHead, newEdgeTail) = safeUncons(newEdgeStr)
                                            val newMap: ParMap = ParMap(
                                              SortedParMap(
                                                Seq[(Par, Par)](
                                                  parByteArray(oldEdgeHead) -> ETuple(
                                                    Seq(ps(0), parByteArray(oldEdgeTail), ps(2))),
                                                  parByteArray(newEdgeHead) -> ETuple(
                                                    Seq(GInt(0),
                                                        parByteArray(newEdgeTail),
                                                        valuePar))
                                                )))
                                            val newName: Par = GPrivate(
                                              ByteString.copyFrom(callRand.next()))
                                            val updatedTuple: Par =
                                              ETuple(Seq(GInt(1), outgoingEdge, newName))
                                            val updatedMap: Par = ParMap(
                                              SortedParMap(
                                                parMap.ps + (parByteArray(head) -> updatedTuple)))
                                            for {
                                              _ <- replace(Quote(updatedMap), replaceChan, dataRand)
                                              _ <- replace(Quote(newMap),
                                                           Quote(newName),
                                                           callRand.splitByte(0))
                                              _ <- succeed(ret, valuePar, callRand.splitByte(1))
                                            } yield ()
                                          case _ =>
                                            localFail()
                                        }
                                      ps(0).singleExpr() match {
                                        case Some(Expr(GInt(0))) =>
                                          // Replace key
                                          if (tail == edgeAdditional) {
                                            insert()
                                          } else {
                                            split()
                                          }
                                        case Some(Expr(GInt(1))) =>
                                          // If we have a complete match, recurse.
                                          if (tail.startsWith(edgeAdditional)) {
                                            val newKey = tail.substring(edgeAdditional.size)

                                            replace(data, replaceChan, dataRand).flatMap(
                                              _ =>
                                                fetchDataInsert(
                                                  Quote(ps(2)),
                                                  Channel(Quote(parByteArray(newKey))),
                                                  value,
                                                  ret,
                                                  callRand))
                                          } else {
                                            split()
                                          }
                                        case _ => localFail()
                                      }
                                    case _ => localFail()
                                  }
                                }
                              case _ => localFail()
                            }
                        }
                      }
                      case _ => localFail()
                    }
                  case _ => localFail()
                }
              case _ => localFail()
            }
          case _ => localFail()
        }
      case _ => Task.unit
    }
  }

  def delete(args: RootSeq[ListChannelWithRandom]): Task[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, ret), rand, cost)) =>
        key match {
          case Channel(Quote(keyPar)) =>
            keyPar.singleExpr match {
              case Some(Expr(GByteArray(_))) =>
                fetchDataRootDelete(Quote(registryRoot), key, ret, rand)
              case _ =>
                fail(ret, rand)
            }
          case _ =>
            fail(ret, rand)
        }
      case _ => Task.unit
    }

  def deleteRootCallback(args: RootSeq[ListChannelWithRandom]): Task[Unit] =
    args match {
      case Seq(ListChannelWithRandom(Seq(key, ret, replaceChan), callRand, callCost),
               ListChannelWithRandom(Seq(data), dataRand, dataCost)) =>
        def localFail() = failAndReplace(data, replaceChan, ret, dataRand, callRand)
        key match {
          case Channel(Quote(keyPar)) =>
            keyPar.singleExpr match {
              case Some(Expr(GByteArray(bs))) =>
                val (head, tail) = safeUncons(bs)
                data match {
                  case Channel(Quote(dataPar)) =>
                    dataPar.singleExpr match {
                      case Some(Expr(EMapBody(parMap))) =>
                        parMap.ps.get(parByteArray(head)) match {
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
                                          if (tail == edgeAdditional) {
                                            val updatedMap: Par = ParMap(
                                              SortedParMap(parMap.ps - parByteArray(head)))
                                            replace(Quote(updatedMap), replaceChan, dataRand)
                                              .flatMap(_ => succeed(ret, ps(2), callRand))
                                          } else {
                                            localFail()
                                          }
                                        case Some(Expr(GInt(1))) =>
                                          if (tail.startsWith(edgeAdditional)) {
                                            val newKey = tail.substring(edgeAdditional.size)
                                            fetchDataDelete(Quote(ps(2)),
                                                            Channel(Quote(parByteArray(newKey))),
                                                            ret,
                                                            callRand,
                                                            Quote(parByteArray(head)),
                                                            data,
                                                            replaceChan,
                                                            dataRand)
                                          } else {
                                            localFail()
                                          }
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
              case _ => localFail()
            }
          case _ => localFail()
        }
      case _ => Task.unit
    }

  def deleteCallback(args: RootSeq[ListChannelWithRandom]): Task[Unit] =
    args match {
      case Seq(
          ListChannelWithRandom(Seq(key, ret, replaceChan), callRand, callCost),
          ListChannelWithRandom(Seq(parentKey, parentData, parentReplace), parentRand, parentCost),
          ListChannelWithRandom(Seq(data), dataRand, dataCost)) =>
        def localFail() =
          replace(parentData, parentReplace, parentRand).flatMap(_ =>
            failAndReplace(data, replaceChan, ret, dataRand, callRand))
        key match {
          case Channel(Quote(keyPar)) =>
            keyPar.singleExpr match {
              case Some(Expr(GByteArray(bs))) =>
                val (head, tail) = safeUncons(bs)

                // This nests deeply, and it's easier to read pulled out.
                def mergeWithParent(lastKey: Par, lastEntry: Par): Task[Unit] =
                  parentKey match {
                    case Channel(Quote(parentKeyPar)) =>
                      parentData match {
                        case Channel(Quote(parentDataPar)) =>
                          parentDataPar.singleExpr() match {
                            case Some(Expr(EMapBody(parMap))) =>
                              parMap.ps.get(parentKeyPar) match {
                                case None => localFail()
                                case Some(parentEntry) =>
                                  parentEntry.singleExpr() match {
                                    case Some(Expr(ETupleBody(ETuple(parentPs, _, _)))) =>
                                      if (parentPs.length != 3)
                                        localFail()
                                      else
                                        parentPs(1).singleExpr match {
                                          case Some(Expr(GByteArray(parentAdditional))) =>
                                            lastKey.singleExpr() match {
                                              case Some(Expr(GByteArray(lastKeyStr))) =>
                                                lastEntry.singleExpr() match {
                                                  case Some(Expr(ETupleBody(ETuple(ps, _, _)))) =>
                                                    if (ps.length != 3)
                                                      localFail()
                                                    else
                                                      ps(1).singleExpr() match {
                                                        case Some(
                                                            Expr(GByteArray(edgeAdditional))) =>
                                                          val mergeStream = ByteString.newOutput()
                                                          parentAdditional.writeTo(mergeStream)
                                                          lastKeyStr.writeTo(mergeStream)
                                                          edgeAdditional.writeTo(mergeStream)
                                                          val mergedEdge = parByteArray(
                                                            mergeStream.toByteString())
                                                          val updatedTuple: Par =
                                                            ETuple(Seq(ps(0), mergedEdge, ps(2)))
                                                          val updatedMap: Par = ParMap(
                                                            SortedParMap(
                                                              parMap.ps +
                                                                (parentKeyPar -> updatedTuple)))
                                                          replace(Quote(updatedMap),
                                                                  parentReplace,
                                                                  parentRand)
                                                      }
                                                  case _ => localFail()
                                                  case _ => localFail()
                                                }
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
                    case _ => localFail()
                  }

                data match {
                  case Channel(Quote(dataPar)) =>
                    dataPar.singleExpr match {
                      case Some(Expr(EMapBody(parMap))) =>
                        parMap.ps.get(parByteArray(head)) match {
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
                                          if (tail == edgeAdditional) {
                                            if (parMap.ps.size > 2) {
                                              val updatedMap: Par = ParMap(
                                                SortedParMap(parMap.ps - parByteArray(head)))
                                              for {
                                                _ <- replace(Quote(updatedMap),
                                                             replaceChan,
                                                             dataRand)
                                                _ <- replace(parentData, parentReplace, parentRand)
                                                _ <- succeed(ret, ps(2), callRand)
                                              } yield ()
                                            } else if (parMap.ps.size != 2) {
                                              localFail()
                                            } else {
                                              val it = (parMap.ps - parByteArray(head)).iterator
                                              for {
                                                _ <- Function.tupled(mergeWithParent(_, _))(it.next)
                                                _ <- succeed(ret, ps(2), callRand)
                                              } yield ()
                                            }
                                          } else {
                                            localFail()
                                          }
                                        case Some(Expr(GInt(1))) =>
                                          if (tail.startsWith(edgeAdditional)) {
                                            val newKey = tail.substring(edgeAdditional.size)
                                            fetchDataDelete(Quote(ps(2)),
                                                            Channel(Quote(parByteArray(newKey))),
                                                            ret,
                                                            callRand,
                                                            Quote(parByteArray(head)),
                                                            data,
                                                            replaceChan,
                                                            dataRand)
                                          } else {
                                            localFail()
                                          }
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
              case _ => localFail()
            }
          case _ => localFail()
        }
      case _ => Task.unit
    }

  val testingDispatchTable: Map[Long, Function1[RootSeq[ListChannelWithRandom], Task[Unit]]] =
    Map(
      lookupRef             -> lookup,
      lookupCallbackRef     -> lookupCallback,
      insertRef             -> insert,
      insertCallbackRef     -> insertCallback,
      deleteRef             -> delete,
      deleteRootCallbackRef -> deleteRootCallback,
      deleteCallbackRef     -> deleteCallback
    )
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
