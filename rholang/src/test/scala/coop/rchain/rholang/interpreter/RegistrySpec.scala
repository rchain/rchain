package coop.rchain.rholang.interpreter

import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace._
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.Await
import scala.concurrent.duration._

trait RegistryTester extends PersistentStoreTester {
  implicit val errorLog = new ErrorLog()
  implicit val costAccounting =
    CostAccountingAlg.unsafe[Task](CostAccount.zero)
  def withRegistryAndTestSpace[R](
      f: (Reduce[Task], 
          ISpace[Channel,
                 BindPattern,
                 ListChannelWithRandom,
                 ListChannelWithRandom,
                 TaggedContinuation]) => R
          ): R = {
    withTestSpace { space =>
      lazy val registry: Registry = new Registry(space, dispatcher)
      lazy val dispatcher: Dispatch[Task, ListChannelWithRandom, TaggedContinuation] =
        RholangAndScalaDispatcher.create[Task, Task.Par](space, registry.testingDispatchTable, Map.empty)
      val reducer = dispatcher.reducer
      // This looks pointless, but it forces the creation of the registry.
      registry.testInstall()
      f(reducer, space)
    }
  }
}

class RegistrySpec extends FlatSpec with Matchers with RegistryTester {
  val eightByteArray: Par       = GByteArray(ByteString.copyFrom(Array[Byte](0x08.toByte)))
  val ninetySevenByteArray: Par = GByteArray(ByteString.copyFrom(Array[Byte](0x97.toByte)))
  val branchName: Par = GPrivate(
    ByteString.copyFrom(
      Base16.decode("d5fd3d8daf9f295aa590b37b50d5518803b4596ed6940aa42d46b1413a1bb16e")))
  val rootSend: Send = Send(
    chan = Quote(Registry.registryRoot),
    data = Seq(
      Expr(EMapBody(
        ParMap(Seq(eightByteArray -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(1), ninetySevenByteArray, branchName))))))))))),
    persistent = false
  )
  val sixCByteArray: Par   = GByteArray(ByteString.copyFrom(Array[Byte](0x6c.toByte)))
  val dcByteArray: Par     = GByteArray(ByteString.copyFrom(Array[Byte](0xdc.toByte)))
  val eSevenByteArray: Par = GByteArray(ByteString.copyFrom(Array[Byte](0xe7.toByte)))
  val eNineByteArray: Par  = GByteArray(ByteString.copyFrom(Array[Byte](0xe9.toByte)))
  val emptyByteArray: Par  = GByteArray(ByteString.EMPTY)
  val branch1: Par         = GByteArray(ByteString.copyFrom(Base16.decode("bcb195efbad3b7ca343f50713e")))
  val branch2: Par         = GByteArray(ByteString.copyFrom(Base16.decode("75426a859c8d3b25df4508cda9")))
  val branch3: Par         = GByteArray(ByteString.copyFrom(Base16.decode("55241ea248915d153e082c75a9")))
  val branch4: Par         = GByteArray(ByteString.copyFrom(Base16.decode("533fd9c5c26e7ea3fe07f99a4d")))
  val branchSend: Send = Send(
    chan = Quote(branchName),
    data = Seq(
      Expr(EMapBody(ParMap(Seq(
        emptyByteArray  -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), emptyByteArray, GInt(7))))))),
        dcByteArray     -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch1, GInt(8))))))),
        eSevenByteArray -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch2, GInt(9)))))))
      ))))),
    persistent = false
  )

  val fullBranchSend: Send = Send(
    chan = Quote(branchName),
    data = Seq(
      Expr(EMapBody(ParMap(Seq(
        dcByteArray     -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch1, GInt(8))))))),
        eSevenByteArray -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch2, GInt(9))))))),
        sixCByteArray   -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch3, GInt(10))))))),
        eNineByteArray  -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch4, GInt(11)))))))
      ))))),
    persistent = false
  )

  val baseRand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])
  "lookup" should "recurse" in {
    /* lookup par is effectively:
      new r(`rho:registry:lookup`) in {
        r!("0897dcbcb195efbad3b7ca343f50713e".hexToBytes(), "result0") |
        r!("0897e775426a859c8d3b25df4508cda9".hexToBytes(), "result1") |
        r!("0897".hexToBytes(), "result2")
      }
      But with the effect of the new already applied.
    */
    val lookupPar: Par = Par.fromAscii("""
sends {
  chan {
    quote {
      ids {
        id: "\012"
      }
    }
  }
  data {
    exprs {
      e_method_body {
        methodName: "hexToBytes"
        target {
          exprs {
            g_string: "0897"
          }
          locallyFree: "\000\000\000\000\000\000\000\000"
        }
        locallyFree: "\000\000\000\000\000\000\000\000"
      }
    }
    locallyFree: "\000\000\000\000\000\000\000\000"
  }
  data {
    exprs {
      g_string: "result0"
    }
    locallyFree: "\000\000\000\000\000\000\000\000"
  }
  persistent: false
  locallyFree: "\000\000\000\000\000\000\000\000"
}
sends {
  chan {
    quote {
      ids {
        id: "\012"
      }
    }
  }
  data {
    exprs {
      e_method_body {
        methodName: "hexToBytes"
        target {
          exprs {
            g_string: "0897dcbcb195efbad3b7ca343f50713e"
          }
          locallyFree: "\000\000\000\000\000\000\000\000"
          connective_used: false
        }
        locallyFree: "\000\000\000\000\000\000\000\000"
        connective_used: false
      }
    }
    locallyFree: "\000\000\000\000\000\000\000\000"
    connective_used: false
  }
  data {
    exprs {
      g_string: "result1"
    }
    locallyFree: "\000\000\000\000\000\000\000\000"
    connective_used: false
  }
  persistent: false
  locallyFree: "\000\000\000\000\000\000\000\000"
  connective_used: false
}
sends {
  chan {
    quote {
      ids {
        id: "\012"
      }
    }
  }
  data {
    exprs {
      e_method_body {
        methodName: "hexToBytes"
        target {
          exprs {
            g_string: "0897e775426a859c8d3b25df4508cda9"
          }
          locallyFree: "\000\000\000\000\000\000\000\000"
          connective_used: false
        }
        locallyFree: "\000\000\000\000\000\000\000\000"
        connective_used: false
      }
    }
    locallyFree: "\000\000\000\000\000\000\000\000"
    connective_used: false
  }
  data {
    exprs {
      g_string: "result2"
    }
    locallyFree: "\000\000\000\000\000\000\000\000"
    connective_used: false
  }
  persistent: false
  locallyFree: "\000\000\000\000\000\000\000\000"
  connective_used: false
}
locallyFree: "\000\000\000\000\000\000\000\000"
connective_used: false""")

    val completePar                     = lookupPar.addSends(rootSend, branchSend)
    implicit val rand: Blake2b512Random = baseRand.splitByte(1)
    val randResult0                     = rand.splitByte(0)
    randResult0.next; randResult0.next
    val randResult1 = rand.splitByte(1)
    randResult1.next; randResult1.next
    val randResult2 = rand.splitByte(2)
    randResult2.next; randResult2.next

    val result = withRegistryAndTestSpace { (reducer, space) =>
      implicit val env    = Env[Par]()
      val resultTask = for {
        _ <- reducer.eval(completePar)
      } yield space.store.toMap
      Await.result(resultTask.runAsync, 3.seconds)
    }

    def resultChanList(s: String) =
      List(Channel(Quote(GString(s))))

    result.get(resultChanList("result0")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result0"))),
                       ListChannelWithRandom(Seq(Quote(GInt(7))), randResult0),
                       false)),
        List()
      )))
    result.get(resultChanList("result1")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result1"))),
                       ListChannelWithRandom(Seq(Quote(GInt(8))), randResult1),
                       false)),
        List()
      )))
    result.get(resultChanList("result2")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result2"))),
                       ListChannelWithRandom(Seq(Quote(GInt(9))), randResult2),
                       false)),
        List()
      )))
  }

  "insert" should "successfully split" in {
    /* Insert Par is effectively:
        new rl(`rho:registry:lookup`), ri(`rho:registry:insert`) in {
          new ack in {
            ri!("0897dcbc".hexToBytes(), 10, *ack) |
            for (@10 <- ack) { //merge 0
              rl!("0897dcbcb195efbad3b7ca343f50713e".hexToBytes(), *ack) |
              for (@x <- ack) { //merge 1
                @"result0"!(x) |
                rl!("0897dcbc".hexToBytes(), *ack) |
                for (@x <- ack) { //merge 2
                  @"result1"!(x) |
                  ri!("0897dc".hexToBytes(), 11, *ack) |
                  for (@11 <- ack) { //merge 3
                    rl!("0897dcbcb195efbad3b7ca343f50713e".hexToBytes(), *ack) |
                    for (@x <- ack) { //merge4
                      @"result2"!(x) |
                      rl!("0897dcbc".hexToBytes(), *ack) |
                      for (@x <- ack) { //merge5
                        @"result3"!(x) |
                        rl!("0897dc".hexToBytes(), *ack) |
                        for (@x <- ack) {
                          @"result4"!(x) |
                          ri!("08bb".hexToBytes(), 12, *ack) |
                          for (@12 <- ack) {
                            rl!("0897dcbcb195efbad3b7ca343f50713e".hexToBytes(), "result5") |
                            rl!("0897e775426a859c8d3b25df4508cda9".hexToBytes(), "result6") |
                            rl!("0897".hexToBytes(), "result7") |
                            rl!("0897dc".hexToBytes(), "result8") |
                            rl!("0897dcbc".hexToBytes(), "result9") |
                            rl!("08bb".hexToBytes(), "result10")
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
    But without the urn's */
    val insertPar: Par = Par.fromAscii("""
news {
  bindCount: 1
  p {
    sends {
      chan {
        quote {
          ids {
            id: "\014"
          }
        }
      }
      data {
        exprs {
          e_method_body {
            methodName: "hexToBytes"
            target {
              exprs {
                g_string: "0897dcbc"
              }
              locallyFree: "\000\000\000\000\000\000\000\000"
              connective_used: false
            }
            locallyFree: "\000\000\000\000\000\000\000\000"
            connective_used: false
          }
        }
        locallyFree: "\000\000\000\000\000\000\000\000"
        connective_used: false
      }
      data {
        exprs {
          g_int: 10
        }
        locallyFree: "\000\000\000\000\000\000\000\000"
        connective_used: false
      }
      data {
        exprs {
          e_eval_body {
            chanVar {
              bound_var: 0
            }
          }
        }
        locallyFree: "\001\000\000\000\000\000\000\000"
        connective_used: false
      }
      persistent: false
      locallyFree: "\001\000\000\000\000\000\000\000"
      connective_used: false
    }
    receives {
      binds {
        patterns {
          quote {
            exprs {
              g_int: 10
            }
            locallyFree: "\000\000\000\000\000\000\000\000"
            connective_used: false
          }
        }
        source {
          chanVar {
            bound_var: 0
          }
        }
        freeCount: 0
      }
      body {
        sends {
          chan {
            quote {
              ids {
                id: "\012"
              }
            }
          }
          data {
            exprs {
              e_method_body {
                methodName: "hexToBytes"
                target {
                  exprs {
                    g_string: "0897dcbcb195efbad3b7ca343f50713e"
                  }
                  locallyFree: "\000\000\000\000\000\000\000\000"
                  connective_used: false
                }
                locallyFree: "\000\000\000\000\000\000\000\000"
                connective_used: false
              }
            }
            locallyFree: "\000\000\000\000\000\000\000\000"
            connective_used: false
          }
          data {
            exprs {
              e_eval_body {
                chanVar {
                  bound_var: 0
                }
              }
            }
            locallyFree: "\001\000\000\000\000\000\000\000"
            connective_used: false
          }
          persistent: false
          locallyFree: "\001\000\000\000\000\000\000\000"
          connective_used: false
        }
        receives {
          binds {
            patterns {
              quote {
                exprs {
                  e_var_body {
                    v {
                      free_var: 0
                    }
                  }
                }
                locallyFree: "\000\000\000\000\000\000\000\000"
                connective_used: true
              }
            }
            source {
              chanVar {
                bound_var: 0
              }
            }
            freeCount: 1
          }
          body {
            sends {
              chan {
                quote {
                  exprs {
                    g_string: "result0"
                  }
                  locallyFree: "\000\000\000\000\000\000\000\000"
                  connective_used: false
                }
              }
              data {
                exprs {
                  e_var_body {
                    v {
                      bound_var: 0
                    }
                  }
                }
                locallyFree: "\001\000\000\000\000\000\000\000"
                connective_used: false
              }
              persistent: false
              locallyFree: "\001\000\000\000\000\000\000\000"
              connective_used: false
            }
            sends {
              chan {
                quote {
                  ids {
                    id: "\012"
                  }
                }
              }
              data {
                exprs {
                  e_method_body {
                    methodName: "hexToBytes"
                    target {
                      exprs {
                        g_string: "0897dcbc"
                      }
                      locallyFree: "\000\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                    locallyFree: "\000\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                }
                locallyFree: "\000\000\000\000\000\000\000\000"
                connective_used: false
              }
              data {
                exprs {
                  e_eval_body {
                    chanVar {
                      bound_var: 1
                    }
                  }
                }
                locallyFree: "\002\000\000\000\000\000\000\000"
                connective_used: false
              }
              persistent: false
              locallyFree: "\006\000\000\000\000\000\000\000"
              connective_used: false
            }
            receives {
              binds {
                patterns {
                  quote {
                    exprs {
                      e_var_body {
                        v {
                          free_var: 0
                        }
                      }
                    }
                    locallyFree: "\000\000\000\000\000\000\000\000"
                    connective_used: true
                  }
                }
                source {
                  chanVar {
                    bound_var: 1
                  }
                }
                freeCount: 1
              }
              body {
                sends {
                  chan {
                    quote {
                      exprs {
                        g_string: "result1"
                      }
                      locallyFree: "\000\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                  }
                  data {
                    exprs {
                      e_var_body {
                        v {
                          bound_var: 0
                        }
                      }
                    }
                    locallyFree: "\001\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  persistent: false
                  locallyFree: "\001\000\000\000\000\000\000\000"
                  connective_used: false
                }
                sends {
                  chan {
                    quote {
                      ids {
                        id: "\014"
                      }
                    }
                  }
                  data {
                    exprs {
                      e_method_body {
                        methodName: "hexToBytes"
                        target {
                          exprs {
                            g_string: "0897dc"
                          }
                          locallyFree: "\000\000\000\000\000\000\000\000"
                          connective_used: false
                        }
                        locallyFree: "\000\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                    }
                    locallyFree: "\000\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  data {
                    exprs {
                      g_int: 11
                    }
                    locallyFree: "\000\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  data {
                    exprs {
                      e_eval_body {
                        chanVar {
                          bound_var: 2
                        }
                      }
                    }
                    locallyFree: "\004\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  persistent: false
                  locallyFree: "\024\000\000\000\000\000\000\000"
                  connective_used: false
                }
                receives {
                  binds {
                    patterns {
                      quote {
                        exprs {
                          g_int: 11
                        }
                        locallyFree: "\000\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                    }
                    source {
                      chanVar {
                        bound_var: 2
                      }
                    }
                    freeCount: 0
                  }
                  body {
                    sends {
                      chan {
                        quote {
                          ids {
                            id: "\012"
                          }
                        }
                      }
                      data {
                        exprs {
                          e_method_body {
                            methodName: "hexToBytes"
                            target {
                              exprs {
                                g_string: "0897dcbcb195efbad3b7ca343f50713e"
                              }
                              locallyFree: "\000\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                            locallyFree: "\000\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                        }
                        locallyFree: "\000\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                      data {
                        exprs {
                          e_eval_body {
                            chanVar {
                              bound_var: 2
                            }
                          }
                        }
                        locallyFree: "\004\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                      persistent: false
                      locallyFree: "\f\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                    receives {
                      binds {
                        patterns {
                          quote {
                            exprs {
                              e_var_body {
                                v {
                                  free_var: 0
                                }
                              }
                            }
                            locallyFree: "\000\000\000\000\000\000\000\000"
                            connective_used: true
                          }
                        }
                        source {
                          chanVar {
                            bound_var: 2
                          }
                        }
                        freeCount: 1
                      }
                      body {
                        sends {
                          chan {
                            quote {
                              exprs {
                                g_string: "result2"
                              }
                              locallyFree: "\000\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                          }
                          data {
                            exprs {
                              e_var_body {
                                v {
                                  bound_var: 0
                                }
                              }
                            }
                            locallyFree: "\001\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                          persistent: false
                          locallyFree: "\001\000\000\000\000\000\000\000"
                          connective_used: false
                        }
                        sends {
                          chan {
                            quote {
                              ids {
                                id: "\012"
                              }
                            }
                          }
                          data {
                            exprs {
                              e_method_body {
                                methodName: "hexToBytes"
                                target {
                                  exprs {
                                    g_string: "0897dcbc"
                                  }
                                  locallyFree: "\000\000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                                locallyFree: "\000\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                            }
                            locallyFree: "\000\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                          data {
                            exprs {
                              e_eval_body {
                                chanVar {
                                  bound_var: 3
                                }
                              }
                            }
                            locallyFree: "\b\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                          persistent: false
                          locallyFree: "\030\000\000\000\000\000\000\000"
                          connective_used: false
                        }
                        receives {
                          binds {
                            patterns {
                              quote {
                                exprs {
                                  e_var_body {
                                    v {
                                      free_var: 0
                                    }
                                  }
                                }
                                locallyFree: "\000\000\000\000\000\000\000\000"
                                connective_used: true
                              }
                            }
                            source {
                              chanVar {
                                bound_var: 3
                              }
                            }
                            freeCount: 1
                          }
                          body {
                            sends {
                              chan {
                                quote {
                                  exprs {
                                    g_string: "result3"
                                  }
                                  locallyFree: "\000\000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                              }
                              data {
                                exprs {
                                  e_var_body {
                                    v {
                                      bound_var: 0
                                    }
                                  }
                                }
                                locallyFree: "\001\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                              persistent: false
                              locallyFree: "\001\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                            sends {
                              chan {
                                quote {
                                  ids {
                                    id: "\012"
                                  }
                                }
                              }
                              data {
                                exprs {
                                  e_method_body {
                                    methodName: "hexToBytes"
                                    target {
                                      exprs {
                                        g_string: "0897dc"
                                      }
                                      locallyFree: "\000\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    locallyFree: "\000\000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                }
                                locallyFree: "\000\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                              data {
                                exprs {
                                  e_eval_body {
                                    chanVar {
                                      bound_var: 4
                                    }
                                  }
                                }
                                locallyFree: "\020\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                              persistent: false
                              locallyFree: "0\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                            receives {
                              binds {
                                patterns {
                                  quote {
                                    exprs {
                                      e_var_body {
                                        v {
                                          free_var: 0
                                        }
                                      }
                                    }
                                    locallyFree: "\000\000\000\000\000\000\000\000"
                                    connective_used: true
                                  }
                                }
                                source {
                                  chanVar {
                                    bound_var: 4
                                  }
                                }
                                freeCount: 1
                              }
                              body {
                                sends {
                                  chan {
                                    quote {
                                      exprs {
                                        g_string: "result4"
                                      }
                                      locallyFree: "\000\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                  }
                                  data {
                                    exprs {
                                      e_var_body {
                                        v {
                                          bound_var: 0
                                        }
                                      }
                                    }
                                    locallyFree: "\001\000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  persistent: false
                                  locallyFree: "\001\000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                                sends {
                                  chan {
                                    quote {
                                      ids {
                                        id: "\014"
                                      }
                                    }
                                  }
                                  data {
                                    exprs {
                                      e_method_body {
                                        methodName: "hexToBytes"
                                        target {
                                          exprs {
                                            g_string: "08bb"
                                          }
                                          locallyFree: "\000\000\000\000\000\000\000\000"
                                          connective_used: false
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                    }
                                    locallyFree: "\000\000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  data {
                                    exprs {
                                      g_int: 12
                                    }
                                    locallyFree: "\000\000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  data {
                                    exprs {
                                      e_eval_body {
                                        chanVar {
                                          bound_var: 5
                                        }
                                      }
                                    }
                                    locallyFree: " \000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  persistent: false
                                  locallyFree: "\240\000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                                receives {
                                  binds {
                                    patterns {
                                      quote {
                                        exprs {
                                          g_int: 12
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                    }
                                    source {
                                      chanVar {
                                        bound_var: 5
                                      }
                                    }
                                    freeCount: 0
                                  }
                                  body {
                                    sends {
                                      chan {
                                        quote {
                                          ids {
                                            id: "\012"
                                          }
                                        }
                                      }
                                      data {
                                        exprs {
                                          e_method_body {
                                            methodName: "hexToBytes"
                                            target {
                                              exprs {
                                                g_string: "0897"
                                              }
                                              locallyFree: "\000\000\000\000\000\000\000\000"
                                              connective_used: false
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      data {
                                        exprs {
                                          g_string: "result7"
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      persistent: false
                                      locallyFree: "@\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    sends {
                                      chan {
                                        quote {
                                          ids {
                                            id: "\012"
                                          }
                                        }
                                      }
                                      data {
                                        exprs {
                                          e_method_body {
                                            methodName: "hexToBytes"
                                            target {
                                              exprs {
                                                g_string: "0897dc"
                                              }
                                              locallyFree: "\000\000\000\000\000\000\000\000"
                                              connective_used: false
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      data {
                                        exprs {
                                          g_string: "result8"
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      persistent: false
                                      locallyFree: "@\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    sends {
                                      chan {
                                        quote {
                                          ids {
                                            id: "\012"
                                          }
                                        }
                                      }
                                      data {
                                        exprs {
                                          e_method_body {
                                            methodName: "hexToBytes"
                                            target {
                                              exprs {
                                                g_string: "0897dcbc"
                                              }
                                              locallyFree: "\000\000\000\000\000\000\000\000"
                                              connective_used: false
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      data {
                                        exprs {
                                          g_string: "result9"
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      persistent: false
                                      locallyFree: "@\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    sends {
                                      chan {
                                        quote {
                                          ids {
                                            id: "\012"
                                          }
                                        }
                                      }
                                      data {
                                        exprs {
                                          e_method_body {
                                            methodName: "hexToBytes"
                                            target {
                                              exprs {
                                                g_string: "0897dcbcb195efbad3b7ca343f50713e"
                                              }
                                              locallyFree: "\000\000\000\000\000\000\000\000"
                                              connective_used: false
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      data {
                                        exprs {
                                          g_string: "result5"
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      persistent: false
                                      locallyFree: "@\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    sends {
                                      chan {
                                        quote {
                                          ids {
                                            id: "\012"
                                          }
                                        }
                                      }
                                      data {
                                        exprs {
                                          e_method_body {
                                            methodName: "hexToBytes"
                                            target {
                                              exprs {
                                                g_string: "0897e775426a859c8d3b25df4508cda9"
                                              }
                                              locallyFree: "\000\000\000\000\000\000\000\000"
                                              connective_used: false
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      data {
                                        exprs {
                                          g_string: "result6"
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      persistent: false
                                      locallyFree: "@\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    sends {
                                      chan {
                                        quote {
                                          ids {
                                            id: "\012"
                                          }
                                        }
                                      }
                                      data {
                                        exprs {
                                          e_method_body {
                                            methodName: "hexToBytes"
                                            target {
                                              exprs {
                                                g_string: "08bb"
                                              }
                                              locallyFree: "\000\000\000\000\000\000\000\000"
                                              connective_used: false
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      data {
                                        exprs {
                                          g_string: "result10"
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      persistent: false
                                      locallyFree: "@\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    locallyFree: "@\000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  persistent: false
                                  bindCount: 0
                                  locallyFree: "\040\000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                                locallyFree: "\041\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                              persistent: false
                              bindCount: 1
                              locallyFree: "\020\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                            locallyFree: "\021\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                          persistent: false
                          bindCount: 1
                          locallyFree: "\010\000\000\000\000\000\000\000"
                          connective_used: false
                        }
                        locallyFree: "\011\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                      persistent: false
                      bindCount: 1
                      locallyFree: "\004\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                    locallyFree: "\004\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  persistent: false
                  bindCount: 0
                  locallyFree: "\004\000\000\000\000\000\000\000"
                  connective_used: false
                }
                locallyFree: "\005\000\000\000\000\000\000\000"
                connective_used: false
              }
              persistent: false
              bindCount: 1
              locallyFree: "\002\000\000\000\000\000\000\000"
              connective_used: false
            }
            locallyFree: "\003\000\000\000\000\000\000\000"
            connective_used: false
          }
          persistent: false
          bindCount: 1
          locallyFree: "\001\000\000\000\000\000\000\000"
          connective_used: false
        }
        locallyFree: "\001\000\000\000\000\000\000\000"
        connective_used: false
      }
      persistent: false
      bindCount: 0
      locallyFree: "\001\000\000\000\000\000\000\000"
      connective_used: false
    }
    locallyFree: "\001\000\000\000\000\000\000\000"
    connective_used: false
  }
  locallyFree: "\000\000\000\000\000\000\000\000"
}""")
    val completePar                     = insertPar.addSends(rootSend, branchSend)
    implicit val evalRand: Blake2b512Random = baseRand.splitByte(2)

    // Compute the random states for the results
    val resultRand = baseRand.splitByte(2)
    // The two sends setting up the initial state, and then the new.
    val newRand = resultRand.splitByte(2)
    // once, for ack.
    newRand.next()
    val insert0Rand = newRand.splitByte(0)
    // Twice for lookup, once for new name to insert.
    insert0Rand.next(); insert0Rand.next(); insert0Rand.next()
    val merge0 = Blake2b512Random.merge(Seq(newRand.splitByte(1), insert0Rand.splitByte(1)))
    val lookup0Rand = merge0.splitByte(0)
    // It takes 3 now because of the previous insert.
    lookup0Rand.next(); lookup0Rand.next(); lookup0Rand.next()
    val merge1 = Blake2b512Random.merge(Seq(merge0.splitByte(1), lookup0Rand))
    val result0Rand = merge1.splitByte(0)
    val lookup1Rand = merge1.splitByte(1)
    lookup1Rand.next(); lookup1Rand.next(); lookup1Rand.next()
    val merge2 = Blake2b512Random.merge(Seq(merge1.splitByte(2), lookup1Rand))
    val result1Rand = merge2.splitByte(0)
    val insert1Rand = merge2.splitByte(1)
    //0897dc only takes 2 lookups (root, and 0897). It uses 1 more to split
    insert1Rand.next(); insert1Rand.next(); insert1Rand.next()
    val merge3 = Blake2b512Random.merge(Seq(merge2.splitByte(2), insert1Rand.splitByte(1)))
    val lookup2Rand = merge3.splitByte(0)
    // It takes 4 lookups now because of the second insert.
    lookup2Rand.next(); lookup2Rand.next(); lookup2Rand.next(); lookup2Rand.next()
    val merge4 = Blake2b512Random.merge(Seq(merge3.splitByte(1), lookup2Rand))
    val result2Rand = merge4.splitByte(0)
    val lookup3Rand = merge4.splitByte(1)
    lookup3Rand.next(); lookup3Rand.next(); lookup3Rand.next(); lookup3Rand.next()
    val merge5 = Blake2b512Random.merge(Seq(merge4.splitByte(2), lookup3Rand))
    val result3Rand = merge5.splitByte(0)
    val lookup4Rand = merge5.splitByte(1)
    //Only 3 lookups: root, 0897, dc
    lookup4Rand.next(); lookup4Rand.next(); lookup4Rand.next()
    val merge6 = Blake2b512Random.merge(Seq(merge5.splitByte(2), lookup4Rand))
    val result4Rand = merge6.splitByte(0)
    val insert2Rand = merge6.splitByte(1)
    // Only 2 because we perform a split at the root
    insert2Rand.next(); insert2Rand.next()
    val merge7 = Blake2b512Random.merge(Seq(merge6.splitByte(2), insert2Rand.splitByte(1)))
    val result7Rand = merge7.splitByte(0)
    result7Rand.next(); result7Rand.next(); result7Rand.next()
    val result8Rand = merge7.splitByte(1)
    result8Rand.next(); result8Rand.next(); result8Rand.next(); result8Rand.next()
    val result9Rand = merge7.splitByte(2)
    result9Rand.next(); result9Rand.next(); result9Rand.next(); result9Rand.next(); result9Rand.next()
    val result5Rand = merge7.splitByte(3)
    result5Rand.next(); result5Rand.next(); result5Rand.next(); result5Rand.next(); result5Rand.next()
    val result6Rand = merge7.splitByte(4)
    result6Rand.next(); result6Rand.next(); result6Rand.next()
    val result10Rand = merge7.splitByte(5)
    result10Rand.next(); result10Rand.next()

    val result = withRegistryAndTestSpace { (reducer, space) =>
      implicit val env    = Env[Par]()
      val resultTask = for {
        _ <- reducer.eval(completePar)
      } yield space.store.toMap
      Await.result(resultTask.runAsync, 3.seconds)
    }

    def resultChanList(s: String) =
      List(Channel(Quote(GString(s))))

    result.get(resultChanList("result0")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result0"))),
                       ListChannelWithRandom(Seq(Quote(GInt(8))), result0Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result1")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result1"))),
                       ListChannelWithRandom(Seq(Quote(GInt(10))), result1Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result2")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result2"))),
                       ListChannelWithRandom(Seq(Quote(GInt(8))), result2Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result3")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result3"))),
                       ListChannelWithRandom(Seq(Quote(GInt(10))), result3Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result4")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result4"))),
                       ListChannelWithRandom(Seq(Quote(GInt(11))), result4Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result5")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result5"))),
                       ListChannelWithRandom(Seq(Quote(GInt(8))), result5Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result6")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result6"))),
                       ListChannelWithRandom(Seq(Quote(GInt(9))), result6Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result7")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result7"))),
                       ListChannelWithRandom(Seq(Quote(GInt(7))), result7Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result8")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result8"))),
                       ListChannelWithRandom(Seq(Quote(GInt(11))), result8Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result9")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result9"))),
                       ListChannelWithRandom(Seq(Quote(GInt(10))), result9Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result10")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result10"))),
                       ListChannelWithRandom(Seq(Quote(GInt(12))), result10Rand),
                       false)),
        List()
      )))
  }

  "delete" should "successfully merge" in {
    /* The overview of this program is 1 delete, 3 lookups, 1 delete,
    2 lookups, 1 delete, 1 lookup, 1 delete 
      delete par is effectively: (but with urn's already substituted)
new rl(`rho:registry:lookup`), rd(`rho:registry:delete`) in {
  new ack in {
    rd!("0897e9533fd9c5c26e7ea3fe07f99a4d".hexToBytes(), *ack) |
    for (@11 <- ack) { //merge 0
      rl!("0897dcbcb195efbad3b7ca343f50713e".hexToBytes(), *ack) |
      for (@x <- ack) { //merge 1
        @"result0"!(x) |
        rl!("0897e775426a859c8d3b25df4508cda9".hexToBytes(), *ack) |
        for (@x <- ack) { //merge 2
          @"result1"!(x) |
          rl!("08976c55241ea248915d153e082c75a9".hexToBytes(), *ack) |
          for (@x <- ack) { //merge 3
            @"result2"!(x) |
            rd!("08976c55241ea248915d153e082c75a9".hexToBytes(), *ack) |
            for (@10 <- ack) { //merge4
              rl!("0897dcbcb195efbad3b7ca343f50713e".hexToBytes(), *ack) |
              for (@x <- ack) { //merge5
                @"result3"!(x) |
                rl!("0897e775426a859c8d3b25df4508cda9".hexToBytes(), *ack) |
                for (@x <- ack) { //merge6
                  @"result4"!(x) |
                  rd!("0897e775426a859c8d3b25df4508cda9".hexToBytes(), *ack) |
                  for (@9 <- ack) { //merge7
                    rl!("0897dcbcb195efbad3b7ca343f50713e".hexToBytes(), *ack) |
                    for (@x <- ack) { //merge8
                      @"result5"!(x) |
                      rd!("0897dcbcb195efbad3b7ca343f50713e".hexToBytes(), "result6")
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
    */
    val deletePar: Par = Par.fromAscii("""
news {
  bindCount: 1
  p {
    sends {
      chan {
        quote {
          ids {
            id: "\016"
          }
        }
      }
      data {
        exprs {
          e_method_body {
            methodName: "hexToBytes"
            target {
              exprs {
                g_string: "0897e9533fd9c5c26e7ea3fe07f99a4d"
              }
              locallyFree: "\000\000\000\000\000\000\000\000"
              connective_used: false
            }
            locallyFree: "\000\000\000\000\000\000\000\000"
            connective_used: false
          }
        }
        locallyFree: "\000\000\000\000\000\000\000\000"
        connective_used: false
      }
      data {
        exprs {
          e_eval_body {
            chanVar {
              bound_var: 0
            }
          }
        }
        locallyFree: "\001\000\000\000\000\000\000\000"
        connective_used: false
      }
      persistent: false
      locallyFree: "\001\000\000\000\000\000\000\000"
      connective_used: false
    }
    receives {
      binds {
        patterns {
          quote {
            exprs {
              g_int: 11
            }
            locallyFree: "\000\000\000\000\000\000\000\000"
            connective_used: false
          }
        }
        source {
          chanVar {
            bound_var: 0
          }
        }
        freeCount: 0
      }
      body {
        sends {
          chan {
            quote {
              ids {
                id: "\012"
              }
            }
          }
          data {
            exprs {
              e_method_body {
                methodName: "hexToBytes"
                target {
                  exprs {
                    g_string: "0897dcbcb195efbad3b7ca343f50713e"
                  }
                  locallyFree: "\000\000\000\000\000\000\000\000"
                  connective_used: false
                }
                locallyFree: "\000\000\000\000\000\000\000\000"
                connective_used: false
              }
            }
            locallyFree: "\000\000\000\000\000\000\000\000"
            connective_used: false
          }
          data {
            exprs {
              e_eval_body {
                chanVar {
                  bound_var: 0
                }
              }
            }
            locallyFree: "\001\000\000\000\000\000\000\000"
            connective_used: false
          }
          persistent: false
          locallyFree: "\001\000\000\000\000\000\000\000"
          connective_used: false
        }
        receives {
          binds {
            patterns {
              quote {
                exprs {
                  e_var_body {
                    v {
                      free_var: 0
                    }
                  }
                }
                locallyFree: "\000\000\000\000\000\000\000\000"
                connective_used: true
              }
            }
            source {
              chanVar {
                bound_var: 0
              }
            }
            freeCount: 1
          }
          body {
            sends {
              chan {
                quote {
                  exprs {
                    g_string: "result0"
                  }
                  locallyFree: "\000\000\000\000\000\000\000\000"
                  connective_used: false
                }
              }
              data {
                exprs {
                  e_var_body {
                    v {
                      bound_var: 0
                    }
                  }
                }
                locallyFree: "\001\000\000\000\000\000\000\000"
                connective_used: false
              }
              persistent: false
              locallyFree: "\001\000\000\000\000\000\000\000"
              connective_used: false
            }
            sends {
              chan {
                quote {
                  ids {
                    id: "\012"
                  }
                }
              }
              data {
                exprs {
                  e_method_body {
                    methodName: "hexToBytes"
                    target {
                      exprs {
                        g_string: "0897e775426a859c8d3b25df4508cda9"
                      }
                      locallyFree: "\000\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                    locallyFree: "\000\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                }
                locallyFree: "\000\000\000\000\000\000\000\000"
                connective_used: false
              }
              data {
                exprs {
                  e_eval_body {
                    chanVar {
                      bound_var: 1
                    }
                  }
                }
                locallyFree: "\002\000\000\000\000\000\000\000"
                connective_used: false
              }
              persistent: false
              locallyFree: "\002\000\000\000\000\000\000\000"
              connective_used: false
            }
            receives {
              binds {
                patterns {
                  quote {
                    exprs {
                      e_var_body {
                        v {
                          free_var: 0
                        }
                      }
                    }
                    locallyFree: "\000\000\000\000\000\000\000\000"
                    connective_used: true
                  }
                }
                source {
                  chanVar {
                    bound_var: 1
                  }
                }
                freeCount: 1
              }
              body {
                sends {
                  chan {
                    quote {
                      exprs {
                        g_string: "result1"
                      }
                      locallyFree: "\000\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                  }
                  data {
                    exprs {
                      e_var_body {
                        v {
                          bound_var: 0
                        }
                      }
                    }
                    locallyFree: "\001\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  persistent: false
                  locallyFree: "\001\000\000\000\000\000\000\000"
                  connective_used: false
                }
                sends {
                  chan {
                    quote {
                      ids {
                        id: "\012"
                      }
                    }
                  }
                  data {
                    exprs {
                      e_method_body {
                        methodName: "hexToBytes"
                        target {
                          exprs {
                            g_string: "08976c55241ea248915d153e082c75a9"
                          }
                          locallyFree: "\000\000\000\000\000\000\000\000"
                          connective_used: false
                        }
                        locallyFree: "\000\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                    }
                    locallyFree: "\000\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  data {
                    exprs {
                      e_eval_body {
                        chanVar {
                          bound_var: 2
                        }
                      }
                    }
                    locallyFree: "\004\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  persistent: false
                  locallyFree: "\004\000\000\000\000\000\000\000"
                  connective_used: false
                }
                receives {
                  binds {
                    patterns {
                      quote {
                        exprs {
                          e_var_body {
                            v {
                              free_var: 0
                            }
                          }
                        }
                        locallyFree: "\000\000\000\000\000\000\000\000"
                        connective_used: true
                      }
                    }
                    source {
                      chanVar {
                        bound_var: 2
                      }
                    }
                    freeCount: 1
                  }
                  body {
                    sends {
                      chan {
                        quote {
                          exprs {
                            g_string: "result2"
                          }
                          locallyFree: "\000\000\000\000\000\000\000\000"
                          connective_used: false
                        }
                      }
                      data {
                        exprs {
                          e_var_body {
                            v {
                              bound_var: 0
                            }
                          }
                        }
                        locallyFree: "\001\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                      persistent: false
                      locallyFree: "\001\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                    sends {
                      chan {
                        quote {
                          ids {
                            id: "\016"
                          }
                        }
                      }
                      data {
                        exprs {
                          e_method_body {
                            methodName: "hexToBytes"
                            target {
                              exprs {
                                g_string: "08976c55241ea248915d153e082c75a9"
                              }
                              locallyFree: "\000\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                            locallyFree: "\000\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                        }
                        locallyFree: "\000\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                      data {
                        exprs {
                          e_eval_body {
                            chanVar {
                              bound_var: 3
                            }
                          }
                        }
                        locallyFree: "\b\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                      persistent: false
                      locallyFree: "\b\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                    receives {
                      binds {
                        patterns {
                          quote {
                            exprs {
                              g_int: 10
                            }
                            locallyFree: "\000\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                        }
                        source {
                          chanVar {
                            bound_var: 3
                          }
                        }
                        freeCount: 0
                      }
                      body {
                        sends {
                          chan {
                            quote {
                              ids {
                                id: "\012"
                              }
                            }
                          }
                          data {
                            exprs {
                              e_method_body {
                                methodName: "hexToBytes"
                                target {
                                  exprs {
                                    g_string: "0897dcbcb195efbad3b7ca343f50713e"
                                  }
                                  locallyFree: "\000\000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                                locallyFree: "\000\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                            }
                            locallyFree: "\000\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                          data {
                            exprs {
                              e_eval_body {
                                chanVar {
                                  bound_var: 3
                                }
                              }
                            }
                            locallyFree: "\b\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                          persistent: false
                          locallyFree: "\b\000\000\000\000\000\000\000"
                          connective_used: false
                        }
                        receives {
                          binds {
                            patterns {
                              quote {
                                exprs {
                                  e_var_body {
                                    v {
                                      free_var: 0
                                    }
                                  }
                                }
                                locallyFree: "\000\000\000\000\000\000\000\000"
                                connective_used: true
                              }
                            }
                            source {
                              chanVar {
                                bound_var: 3
                              }
                            }
                            freeCount: 1
                          }
                          body {
                            sends {
                              chan {
                                quote {
                                  exprs {
                                    g_string: "result3"
                                  }
                                  locallyFree: "\000\000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                              }
                              data {
                                exprs {
                                  e_var_body {
                                    v {
                                      bound_var: 0
                                    }
                                  }
                                }
                                locallyFree: "\001\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                              persistent: false
                              locallyFree: "\001\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                            sends {
                              chan {
                                quote {
                                  ids {
                                    id: "\012"
                                  }
                                }
                              }
                              data {
                                exprs {
                                  e_method_body {
                                    methodName: "hexToBytes"
                                    target {
                                      exprs {
                                        g_string: "0897e775426a859c8d3b25df4508cda9"
                                      }
                                      locallyFree: "\000\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    locallyFree: "\000\000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                }
                                locallyFree: "\000\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                              data {
                                exprs {
                                  e_eval_body {
                                    chanVar {
                                      bound_var: 4
                                    }
                                  }
                                }
                                locallyFree: "\020\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                              persistent: false
                              locallyFree: "\020\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                            receives {
                              binds {
                                patterns {
                                  quote {
                                    exprs {
                                      e_var_body {
                                        v {
                                          free_var: 0
                                        }
                                      }
                                    }
                                    locallyFree: "\000\000\000\000\000\000\000\000"
                                    connective_used: true
                                  }
                                }
                                source {
                                  chanVar {
                                    bound_var: 4
                                  }
                                }
                                freeCount: 1
                              }
                              body {
                                sends {
                                  chan {
                                    quote {
                                      exprs {
                                        g_string: "result4"
                                      }
                                      locallyFree: "\000\000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                  }
                                  data {
                                    exprs {
                                      e_var_body {
                                        v {
                                          bound_var: 0
                                        }
                                      }
                                    }
                                    locallyFree: "\001\000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  persistent: false
                                  locallyFree: "\001\000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                                sends {
                                  chan {
                                    quote {
                                      ids {
                                        id: "\016"
                                      }
                                    }
                                  }
                                  data {
                                    exprs {
                                      e_method_body {
                                        methodName: "hexToBytes"
                                        target {
                                          exprs {
                                            g_string: "0897e775426a859c8d3b25df4508cda9"
                                          }
                                          locallyFree: "\000\000\000\000\000\000\000\000"
                                          connective_used: false
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                    }
                                    locallyFree: "\000\000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  data {
                                    exprs {
                                      e_eval_body {
                                        chanVar {
                                          bound_var: 5
                                        }
                                      }
                                    }
                                    locallyFree: " \000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  persistent: false
                                  locallyFree: " \000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                                receives {
                                  binds {
                                    patterns {
                                      quote {
                                        exprs {
                                          g_int: 9
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                    }
                                    source {
                                      chanVar {
                                        bound_var: 5
                                      }
                                    }
                                    freeCount: 0
                                  }
                                  body {
                                    sends {
                                      chan {
                                        quote {
                                          ids {
                                            id: "\012"
                                          }
                                        }
                                      }
                                      data {
                                        exprs {
                                          e_method_body {
                                            methodName: "hexToBytes"
                                            target {
                                              exprs {
                                                g_string: "0897dcbcb195efbad3b7ca343f50713e"
                                              }
                                              locallyFree: "\000\000\000\000\000\000\000\000"
                                              connective_used: false
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                        }
                                        locallyFree: "\000\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      data {
                                        exprs {
                                          e_eval_body {
                                            chanVar {
                                              bound_var: 5
                                            }
                                          }
                                        }
                                        locallyFree: " \000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      persistent: false
                                      locallyFree: " \000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    receives {
                                      binds {
                                        patterns {
                                          quote {
                                            exprs {
                                              e_var_body {
                                                v {
                                                  free_var: 0
                                                }
                                              }
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: true
                                          }
                                        }
                                        source {
                                          chanVar {
                                            bound_var: 5
                                          }
                                        }
                                        freeCount: 1
                                      }
                                      body {
                                        sends {
                                          chan {
                                            quote {
                                              exprs {
                                                g_string: "result5"
                                              }
                                              locallyFree: "\000\000\000\000\000\000\000\000"
                                              connective_used: false
                                            }
                                          }
                                          data {
                                            exprs {
                                              e_var_body {
                                                v {
                                                  bound_var: 0
                                                }
                                              }
                                            }
                                            locallyFree: "\001\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                          persistent: false
                                          locallyFree: "\001\000\000\000\000\000\000\000"
                                          connective_used: false
                                        }
                                        sends {
                                          chan {
                                            quote {
                                              ids {
                                                id: "\016"
                                              }
                                            }
                                          }
                                          data {
                                            exprs {
                                              e_method_body {
                                                methodName: "hexToBytes"
                                                target {
                                                  exprs {
                                                    g_string: "0897dcbcb195efbad3b7ca343f50713e"
                                                  }
                                                  locallyFree: "\000\000\000\000\000\000\000\000"
                                                  connective_used: false
                                                }
                                                locallyFree: "\000\000\000\000\000\000\000\000"
                                                connective_used: false
                                              }
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                          data {
                                            exprs {
                                              g_string: "result6"
                                            }
                                            locallyFree: "\000\000\000\000\000\000\000\000"
                                            connective_used: false
                                          }
                                          persistent: false
                                          locallyFree: "\000\000\000\000\000\000\000\000"
                                          connective_used: false
                                        }
                                        locallyFree: "\001\000\000\000\000\000\000\000"
                                        connective_used: false
                                      }
                                      persistent: false
                                      bindCount: 1
                                      locallyFree: " \000\000\000\000\000\000\000"
                                      connective_used: false
                                    }
                                    locallyFree: " \000\000\000\000\000\000\000"
                                    connective_used: false
                                  }
                                  persistent: false
                                  bindCount: 0
                                  locallyFree: " \000\000\000\000\000\000\000"
                                  connective_used: false
                                }
                                locallyFree: "!\000\000\000\000\000\000\000"
                                connective_used: false
                              }
                              persistent: false
                              bindCount: 1
                              locallyFree: "\020\000\000\000\000\000\000\000"
                              connective_used: false
                            }
                            locallyFree: "\021\000\000\000\000\000\000\000"
                            connective_used: false
                          }
                          persistent: false
                          bindCount: 1
                          locallyFree: "\b\000\000\000\000\000\000\000"
                          connective_used: false
                        }
                        locallyFree: "\b\000\000\000\000\000\000\000"
                        connective_used: false
                      }
                      persistent: false
                      bindCount: 0
                      locallyFree: "\b\000\000\000\000\000\000\000"
                      connective_used: false
                    }
                    locallyFree: "\t\000\000\000\000\000\000\000"
                    connective_used: false
                  }
                  persistent: false
                  bindCount: 1
                  locallyFree: "\004\000\000\000\000\000\000\000"
                  connective_used: false
                }
                locallyFree: "\005\000\000\000\000\000\000\000"
                connective_used: false
              }
              persistent: false
              bindCount: 1
              locallyFree: "\002\000\000\000\000\000\000\000"
              connective_used: false
            }
            locallyFree: "\003\000\000\000\000\000\000\000"
            connective_used: false
          }
          persistent: false
          bindCount: 1
          locallyFree: "\001\000\000\000\000\000\000\000"
          connective_used: false
        }
        locallyFree: "\001\000\000\000\000\000\000\000"
        connective_used: false
      }
      persistent: false
      bindCount: 0
      locallyFree: "\001\000\000\000\000\000\000\000"
      connective_used: false
    }
    locallyFree: "\001\000\000\000\000\000\000\000"
    connective_used: false
  }
  locallyFree: "\000\000\000\000\000\000\000\000"
}
locallyFree: "\000\000\000\000\000\000\000\000"
connective_used: false
""")
    val completePar                     = deletePar.addSends(rootSend, fullBranchSend)
    implicit val rand: Blake2b512Random = baseRand.splitByte(3)

    val result = withRegistryAndTestSpace { (reducer, space) =>
      implicit val env    = Env[Par]()
      val resultTask = for {
        _ <- reducer.eval(completePar)
      } yield space.store.toMap
      Await.result(resultTask.runAsync, 3.seconds)
    }

    // Compute the random states for the results
    val resultRand = baseRand.splitByte(3)
    // The two sends setting up the initial state, and then the new.
    val newRand = resultRand.splitByte(2)
    val rootRand = resultRand.splitByte(0)
    // once, for ack.
    newRand.next()
    val delete0Rand = newRand.splitByte(0)
    // Once for root, and twice for a single iteration through the tree
    delete0Rand.next(); delete0Rand.next(); delete0Rand.next()
    val merge0 = Blake2b512Random.merge(Seq(newRand.splitByte(1), delete0Rand))
    val lookup0Rand = merge0.splitByte(0)
    lookup0Rand.next(); lookup0Rand.next();
    val merge1 = Blake2b512Random.merge(Seq(merge0.splitByte(1), lookup0Rand))
    val result0Rand = merge1.splitByte(0)
    val lookup1Rand = merge1.splitByte(1)
    lookup1Rand.next(); lookup1Rand.next();
    val merge2 = Blake2b512Random.merge(Seq(merge1.splitByte(2), lookup1Rand))
    val result1Rand = merge2.splitByte(0)
    val lookup2Rand = merge2.splitByte(1)
    lookup2Rand.next(); lookup2Rand.next();
    val merge3 = Blake2b512Random.merge(Seq(merge2.splitByte(2), lookup2Rand))
    val result2Rand = merge3.splitByte(0)
    val delete1Rand = merge3.splitByte(1)
    // Once for root, and twice for a single iteration through the tree
    delete1Rand.next(); delete1Rand.next(); delete1Rand.next()
    val merge4 = Blake2b512Random.merge(Seq(merge3.splitByte(2), delete1Rand))
    val lookup3Rand = merge4.splitByte(0)
    lookup3Rand.next(); lookup3Rand.next();
    val merge5 = Blake2b512Random.merge(Seq(merge4.splitByte(1), lookup3Rand))
    val result3Rand = merge5.splitByte(0)
    val lookup4Rand = merge5.splitByte(1)
    lookup4Rand.next(); lookup4Rand.next();
    val merge6 = Blake2b512Random.merge(Seq(merge5.splitByte(2), lookup4Rand))
    val result4Rand = merge6.splitByte(0)
    val delete2Rand = merge6.splitByte(1)
    // Once for root, and twice for a single iteration through the tree
    delete2Rand.next(); delete2Rand.next(); delete2Rand.next()
    val merge7 = Blake2b512Random.merge(Seq(merge6.splitByte(2), delete2Rand))
    val lookup5Rand = merge7.splitByte(0)
    // The last delete should have merged into the root, so it should only take
    // 1 new name.
    lookup5Rand.next();
    val merge8 = Blake2b512Random.merge(Seq(merge7.splitByte(1), lookup5Rand))
    val result5Rand = merge8.splitByte(0)
    val result6Rand = merge8.splitByte(1)
    // This is the last delete. It should take only 1 lookup, because of the
    // previous merge to root.
    result6Rand.next()

    def resultChanList(s: String) =
      List(Channel(Quote(GString(s))))

    result.get(resultChanList("result0")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result0"))),
                       ListChannelWithRandom(Seq(Quote(GInt(8))), result0Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result1")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result1"))),
                       ListChannelWithRandom(Seq(Quote(GInt(9))), result1Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result2")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result2"))),
                       ListChannelWithRandom(Seq(Quote(GInt(10))), result2Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result3")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result3"))),
                       ListChannelWithRandom(Seq(Quote(GInt(8))), result3Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result4")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result4"))),
                       ListChannelWithRandom(Seq(Quote(GInt(9))), result4Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result5")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result5"))),
                       ListChannelWithRandom(Seq(Quote(GInt(8))), result5Rand),
                       false)),
        List()
      )))
    result.get(resultChanList("result6")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result6"))),
                       ListChannelWithRandom(Seq(Quote(GInt(8))), result6Rand),
                       false)),
        List()
      )))
    result.get(List[Channel](Quote(Registry.registryRoot))) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(Registry.registryRoot)),
                       ListChannelWithRandom(Seq(Quote(EMapBody(ParMap(SortedParMap.empty)))), rootRand),
                       false)),
        List()
      )))
  }
}
