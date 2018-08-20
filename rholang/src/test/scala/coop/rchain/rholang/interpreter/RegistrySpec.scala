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
      registry.testingDispatchTable
      f(reducer, space)
    }
  }
}

class RegistrySpec extends FlatSpec with Matchers with RegistryTester {
  val baseRand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])
  "lookup" should "recurse" in {
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
    val dcByteArray: Par     = GByteArray(ByteString.copyFrom(Array[Byte](0xdc.toByte)))
    val eSevenByteArray: Par = GByteArray(ByteString.copyFrom(Array[Byte](0xe7.toByte)))
    val emptyByteArray: Par  = GByteArray(ByteString.EMPTY)
    val branch1: Par         = GByteArray(ByteString.copyFrom(Base16.decode("bcb195efbad3b7ca343f50713e")))
    val branch2: Par         = GByteArray(ByteString.copyFrom(Base16.decode("75426a859c8d3b25df4508cda9")))
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
  locallyFree: "\001\000\000\000\000\000\000\000"
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
      g_string: "result2"
    }
    locallyFree: "\000\000\000\000\000\000\000\000"
    connective_used: false
  }
  persistent: false
  locallyFree: "\001\000\000\000\000\000\000\000"
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
}
