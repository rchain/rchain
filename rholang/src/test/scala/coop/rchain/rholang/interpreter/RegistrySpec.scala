package coop.rchain.rholang.interpreter

import com.google.protobuf.ByteString
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace._
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import coop.rchain.rspace.pure.PureRSpace
import java.io.StringReader
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.lightningj.util.ZBase32
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.Await
import scala.concurrent.duration._

trait RegistryTester extends PersistentStoreTester {
  implicit val errorLog = new ErrorLog()
  implicit val costAccounting =
    CostAccountingAlg.unsafe[Task](CostAccount.zero)
  def withRegistryAndTestSpace[R](
      f: (Reduce[Task],
          FreudianSpace[Channel,
                        BindPattern,
                        OutOfPhlogistonsError.type,
                        ListChannelWithRandom,
                        CostAccount,
                        ListChannelWithRandom,
                        TaggedContinuation]) => R
  ): R =
    withTestSpace { space =>
      val pureSpace: Runtime.RhoPureSpace = new PureRSpace(space)
      lazy val registry: Registry         = new Registry(pureSpace, dispatcher)
      lazy val dispatcher: Dispatch[Task, ListChannelWithRandom, TaggedContinuation] =
        RholangAndScalaDispatcher
          .create[Task, Task.Par](space, registry.testingDispatchTable, Registry.testingUrnMap)
      val reducer = dispatcher.reducer
      registry.testInstall()
      f(reducer, space)
    }
}

class RegistrySpec extends FlatSpec with Matchers with RegistryTester {
  /*
    0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2
    089775e6bbe6f893b810e66615867bede6e16fcf22a5dd869bb17ca8415f0b8e
    0897ef763354e4266d31c74b7a5be55fbfeb464fe65ce56ce9ccbfd9a1fddef0
    0897a37e047d2fc591185812e4a9526ded5509544e6586092c25a17abf366ea3
   */
  val eightByteArray: Par       = GByteArray(ByteString.copyFrom(Array[Byte](0x08.toByte)))
  val ninetySevenByteArray: Par = GByteArray(ByteString.copyFrom(Array[Byte](0x97.toByte)))
  val branchName: Par = GPrivate(
    ByteString.copyFrom(
      Base16.decode("d5fd3d8daf9f295aa590b37b50d5518803b4596ed6940aa42d46b1413a1bb16e")))
  // format: off
  val rootSend: Send = Send(
    chan = Quote(Registry.registryRoot),
    data = Seq(
      Expr(EMapBody(
        ParMap(Seq(eightByteArray -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(1), ninetySevenByteArray, branchName))))))))))),
    persistent = false
  )
  // format: on
  val sevenFiveByteArray: Par = GByteArray(ByteString.copyFrom(Array[Byte](0x75.toByte)))
  val aThreeByteArray: Par    = GByteArray(ByteString.copyFrom(Array[Byte](0xa3.toByte)))
  val eNineByteArray: Par     = GByteArray(ByteString.copyFrom(Array[Byte](0xe9.toByte)))
  val efByteArray: Par        = GByteArray(ByteString.copyFrom(Array[Byte](0xef.toByte)))
  val emptyByteArray: Par     = GByteArray(ByteString.EMPTY)
  val branch1: Par = GByteArray(
    ByteString.copyFrom(
      Base16.decode("533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2")))
  val branch2: Par = GByteArray(
    ByteString.copyFrom(
      Base16.decode("e6bbe6f893b810e66615867bede6e16fcf22a5dd869bb17ca8415f0b8e")))
  val branch3: Par = GByteArray(
    ByteString.copyFrom(
      Base16.decode("763354e4266d31c74b7a5be55fbfeb464fe65ce56ce9ccbfd9a1fddef0")))
  val branch4: Par = GByteArray(
    ByteString.copyFrom(
      Base16.decode("7e047d2fc591185812e4a9526ded5509544e6586092c25a17abf366ea3")))
  val branchSend: Send = Send(
    chan = Quote(branchName),
    data = Seq(
      Expr(EMapBody(ParMap(Seq(
        emptyByteArray -> Par(
          exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), emptyByteArray, GInt(7))))))),
        eNineByteArray -> Par(
          exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch1, GInt(8))))))),
        sevenFiveByteArray -> Par(
          exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch2, GInt(9)))))))
      ))))),
    persistent = false
  )

  val fullBranchSend: Send = Send(
    chan = Quote(branchName),
    data = Seq(
      Expr(EMapBody(ParMap(Seq(
        eNineByteArray -> Par(
          exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch1, GInt(8))))))),
        sevenFiveByteArray -> Par(
          exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch2, GInt(9))))))),
        efByteArray -> Par(exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch3, GInt(10))))))),
        aThreeByteArray -> Par(
          exprs = Seq(Expr(ETupleBody(ETuple(Seq(GInt(0), branch4, GInt(11)))))))
      ))))),
    persistent = false
  )

  val baseRand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])
  "lookup" should "recurse" in {
    val lookupString: String =
      """
      new r(`rho:registry:testing:lookup`) in {
        r!("0897".hexToBytes(), "result0") |
        r!("089775e6bbe6f893b810e66615867bede6e16fcf22a5dd869bb17ca8415f0b8e".hexToBytes(), "result1") |
        r!("0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2".hexToBytes(), "result2")
      }"""

    val lookupPar: Par = Interpreter.buildNormalizedTerm(new StringReader(lookupString)).value

    val completePar                     = lookupPar.addSends(rootSend, branchSend)
    implicit val rand: Blake2b512Random = baseRand.splitByte(1)
    val newRand                         = rand.splitByte(2)
    val randResult0                     = newRand.splitByte(0)
    randResult0.next; randResult0.next
    val randResult1 = newRand.splitByte(1)
    randResult1.next; randResult1.next
    val randResult2 = newRand.splitByte(2)
    randResult2.next; randResult2.next

    val result = withRegistryAndTestSpace { (reducer, space) =>
      implicit val env = Env[Par]()
      val resultTask = for {
        _ <- reducer.eval(completePar)
      } yield space.store.toMap
      Await.result(resultTask.runAsync, 3.seconds)
    }

    def resultChanList(s: String) =
      List(Channel(Quote(GString(s))))

    result.get(resultChanList("result0")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result0"))),
                            ListChannelWithRandom(Seq(Quote(GInt(7))), randResult0),
                            false)),
          List()
        )))
    result.get(resultChanList("result1")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result1"))),
                            ListChannelWithRandom(Seq(Quote(GInt(9))), randResult1),
                            false)),
          List()
        )))
    result.get(resultChanList("result2")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result2"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), randResult2),
                            false)),
          List()
        )))
  }

  "insert" should "successfully split" in {
    val insertString =
      """
        new rl(`rho:registry:testing:lookup`), ri(`rho:registry:testing:insert`), ack in {
          ri!("0897e953".hexToBytes(), 10, *ack) |
          for (@10 <- ack) { //merge 0
            rl!("0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2".hexToBytes(), *ack) |
            for (@x <- ack) { //merge 1
              @"result0"!(x) |
              rl!("0897e953".hexToBytes(), *ack) |
              for (@x <- ack) { //merge 2
                @"result1"!(x) |
                ri!("0897e9".hexToBytes(), 11, *ack) |
                for (@11 <- ack) { //merge 3
                  rl!("0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2".hexToBytes(), *ack) |
                  for (@x <- ack) { //merge4
                    @"result2"!(x) |
                    rl!("0897e953".hexToBytes(), *ack) |
                    for (@x <- ack) { //merge5
                      @"result3"!(x) |
                      rl!("0897e9".hexToBytes(), *ack) |
                      for (@x <- ack) {
                        @"result4"!(x) |
                        ri!("08bb".hexToBytes(), 12, *ack) |
                        for (@12 <- ack) {
                          rl!("0897".hexToBytes(), "result5") |
                          rl!("089775e6bbe6f893b810e66615867bede6e16fcf22a5dd869bb17ca8415f0b8e".hexToBytes(), "result6") |
                          rl!("0897e9".hexToBytes(), "result7") |
                          rl!("0897e953".hexToBytes(), "result8") |
                          rl!("0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2".hexToBytes(), "result9") |
                          rl!("08bb".hexToBytes(), "result10")
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }"""
    val insertPar: Par                      = Interpreter.buildNormalizedTerm(new StringReader(insertString)).value
    val completePar                         = insertPar.addSends(rootSend, branchSend)
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
    val merge0      = Blake2b512Random.merge(Seq(newRand.splitByte(1), insert0Rand.splitByte(1)))
    val lookup0Rand = merge0.splitByte(0)
    // It takes 3 now because of the previous insert.
    lookup0Rand.next(); lookup0Rand.next(); lookup0Rand.next()
    val merge1      = Blake2b512Random.merge(Seq(merge0.splitByte(1), lookup0Rand))
    val result0Rand = merge1.splitByte(0)
    val lookup1Rand = merge1.splitByte(1)
    lookup1Rand.next(); lookup1Rand.next(); lookup1Rand.next()
    val merge2      = Blake2b512Random.merge(Seq(merge1.splitByte(2), lookup1Rand))
    val result1Rand = merge2.splitByte(0)
    val insert1Rand = merge2.splitByte(1)
    //0897e9 only takes 2 lookups (root, and 0897). It uses 1 more to split
    insert1Rand.next(); insert1Rand.next(); insert1Rand.next()
    val merge3      = Blake2b512Random.merge(Seq(merge2.splitByte(2), insert1Rand.splitByte(1)))
    val lookup2Rand = merge3.splitByte(0)
    // It takes 4 lookups now because of the second insert.
    lookup2Rand.next(); lookup2Rand.next(); lookup2Rand.next(); lookup2Rand.next()
    val merge4      = Blake2b512Random.merge(Seq(merge3.splitByte(1), lookup2Rand))
    val result2Rand = merge4.splitByte(0)
    val lookup3Rand = merge4.splitByte(1)
    lookup3Rand.next(); lookup3Rand.next(); lookup3Rand.next(); lookup3Rand.next()
    val merge5      = Blake2b512Random.merge(Seq(merge4.splitByte(2), lookup3Rand))
    val result3Rand = merge5.splitByte(0)
    val lookup4Rand = merge5.splitByte(1)
    //Only 3 lookups: root, 0897, e9
    lookup4Rand.next(); lookup4Rand.next(); lookup4Rand.next()
    val merge6      = Blake2b512Random.merge(Seq(merge5.splitByte(2), lookup4Rand))
    val result4Rand = merge6.splitByte(0)
    val insert2Rand = merge6.splitByte(1)
    // Only 2 because we perform a split at the root
    insert2Rand.next(); insert2Rand.next()
    val merge7      = Blake2b512Random.merge(Seq(merge6.splitByte(2), insert2Rand.splitByte(1)))
    val result5Rand = merge7.splitByte(0)
    result5Rand.next(); result5Rand.next(); result5Rand.next()
    val result6Rand = merge7.splitByte(1)
    result6Rand.next(); result6Rand.next(); result6Rand.next()
    val result7Rand = merge7.splitByte(2)
    result7Rand.next(); result7Rand.next(); result7Rand.next(); result7Rand.next()
    val result8Rand = merge7.splitByte(3)
    result8Rand.next(); result8Rand.next(); result8Rand.next(); result8Rand.next();
    result8Rand.next()
    val result9Rand = merge7.splitByte(4)
    result9Rand.next(); result9Rand.next(); result9Rand.next(); result9Rand.next();
    result9Rand.next()
    val result10Rand = merge7.splitByte(5)
    result10Rand.next(); result10Rand.next()

    val result = withRegistryAndTestSpace { (reducer, space) =>
      implicit val env = Env[Par]()
      val resultTask = for {
        _ <- reducer.eval(completePar)
      } yield space.store.toMap
      Await.result(resultTask.runAsync, 3.seconds)
    }

    def resultChanList(s: String) =
      List(Channel(Quote(GString(s))))

    result.get(resultChanList("result0")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result0"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), result0Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result1")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result1"))),
                            ListChannelWithRandom(Seq(Quote(GInt(10))), result1Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result2")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result2"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), result2Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result3")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result3"))),
                            ListChannelWithRandom(Seq(Quote(GInt(10))), result3Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result4")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result4"))),
                            ListChannelWithRandom(Seq(Quote(GInt(11))), result4Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result5")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result5"))),
                            ListChannelWithRandom(Seq(Quote(GInt(7))), result5Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result6")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result6"))),
                            ListChannelWithRandom(Seq(Quote(GInt(9))), result6Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result7")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result7"))),
                            ListChannelWithRandom(Seq(Quote(GInt(11))), result7Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result8")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result8"))),
                            ListChannelWithRandom(Seq(Quote(GInt(10))), result8Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result9")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result9"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), result9Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result10")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result10"))),
                            ListChannelWithRandom(Seq(Quote(GInt(12))), result10Rand),
                            false)),
          List()
        )))
  }

  "delete" should "successfully merge" in {
    /* The overview of this program is 1 delete, 3 lookups, 1 delete,
    2 lookups, 1 delete, 1 lookup, 1 delete */
    val deleteString =
      """
      new rl(`rho:registry:testing:lookup`), rd(`rho:registry:testing:delete`), ack in {
        rd!("0897a37e047d2fc591185812e4a9526ded5509544e6586092c25a17abf366ea3".hexToBytes(), *ack) |
        for (@11 <- ack) { //merge 0
          rl!("0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2".hexToBytes(), *ack) |
          for (@x <- ack) { //merge 1
            @"result0"!(x) |
            rl!("089775e6bbe6f893b810e66615867bede6e16fcf22a5dd869bb17ca8415f0b8e".hexToBytes(), *ack) |
            for (@x <- ack) { //merge 2
              @"result1"!(x) |
              rl!("0897ef763354e4266d31c74b7a5be55fbfeb464fe65ce56ce9ccbfd9a1fddef0".hexToBytes(), *ack) |
              for (@x <- ack) { //merge 3
                @"result2"!(x) |
                rd!("0897ef763354e4266d31c74b7a5be55fbfeb464fe65ce56ce9ccbfd9a1fddef0".hexToBytes(), *ack) |
                for (@10 <- ack) { //merge4
                  rl!("0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2".hexToBytes(), *ack) |
                  for (@x <- ack) { //merge5
                    @"result3"!(x) |
                    rl!("089775e6bbe6f893b810e66615867bede6e16fcf22a5dd869bb17ca8415f0b8e".hexToBytes(), *ack) |
                    for (@x <- ack) { //merge6
                      @"result4"!(x) |
                      rd!("089775e6bbe6f893b810e66615867bede6e16fcf22a5dd869bb17ca8415f0b8e".hexToBytes(), *ack) |
                      for (@9 <- ack) { //merge7
                        rl!("0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2".hexToBytes(), *ack) |
                        for (@x <- ack) { //merge8
                          @"result5"!(x) |
                          rd!("0897e9533fd9c5c26e7ea3fe07f99a4dbbde31eb2c59f84810d03e078e7d31c2".hexToBytes(), "result6")
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }"""
    val deletePar: Par                  = Interpreter.buildNormalizedTerm(new StringReader(deleteString)).value
    val completePar                     = deletePar.addSends(rootSend, fullBranchSend)
    implicit val rand: Blake2b512Random = baseRand.splitByte(3)

    val result = withRegistryAndTestSpace { (reducer, space) =>
      implicit val env = Env[Par]()
      val resultTask = for {
        _ <- reducer.eval(completePar)
      } yield space.store.toMap
      Await.result(resultTask.runAsync, 3.seconds)
    }

    // Compute the random states for the results
    val resultRand = baseRand.splitByte(3)
    // The two sends setting up the initial state, and then the new.
    val newRand  = resultRand.splitByte(2)
    val rootRand = resultRand.splitByte(0)
    // once, for ack.
    newRand.next()
    val delete0Rand = newRand.splitByte(0)
    // Once for root, and twice for a single iteration through the tree
    delete0Rand.next(); delete0Rand.next(); delete0Rand.next()
    val merge0      = Blake2b512Random.merge(Seq(newRand.splitByte(1), delete0Rand))
    val lookup0Rand = merge0.splitByte(0)
    lookup0Rand.next(); lookup0Rand.next();
    val merge1      = Blake2b512Random.merge(Seq(merge0.splitByte(1), lookup0Rand))
    val result0Rand = merge1.splitByte(0)
    val lookup1Rand = merge1.splitByte(1)
    lookup1Rand.next(); lookup1Rand.next();
    val merge2      = Blake2b512Random.merge(Seq(merge1.splitByte(2), lookup1Rand))
    val result1Rand = merge2.splitByte(0)
    val lookup2Rand = merge2.splitByte(1)
    lookup2Rand.next(); lookup2Rand.next();
    val merge3      = Blake2b512Random.merge(Seq(merge2.splitByte(2), lookup2Rand))
    val result2Rand = merge3.splitByte(0)
    val delete1Rand = merge3.splitByte(1)
    // Once for root, and twice for a single iteration through the tree
    delete1Rand.next(); delete1Rand.next(); delete1Rand.next()
    val merge4      = Blake2b512Random.merge(Seq(merge3.splitByte(2), delete1Rand))
    val lookup3Rand = merge4.splitByte(0)
    lookup3Rand.next(); lookup3Rand.next();
    val merge5      = Blake2b512Random.merge(Seq(merge4.splitByte(1), lookup3Rand))
    val result3Rand = merge5.splitByte(0)
    val lookup4Rand = merge5.splitByte(1)
    lookup4Rand.next(); lookup4Rand.next();
    val merge6      = Blake2b512Random.merge(Seq(merge5.splitByte(2), lookup4Rand))
    val result4Rand = merge6.splitByte(0)
    val delete2Rand = merge6.splitByte(1)
    // Once for root, and twice for a single iteration through the tree
    delete2Rand.next(); delete2Rand.next(); delete2Rand.next()
    val merge7      = Blake2b512Random.merge(Seq(merge6.splitByte(2), delete2Rand))
    val lookup5Rand = merge7.splitByte(0)
    // The last delete should have merged into the root, so it should only take
    // 1 new name.
    lookup5Rand.next();
    val merge8      = Blake2b512Random.merge(Seq(merge7.splitByte(1), lookup5Rand))
    val result5Rand = merge8.splitByte(0)
    val result6Rand = merge8.splitByte(1)
    // This is the last delete. It should take only 1 lookup, because of the
    // previous merge to root.
    result6Rand.next()

    def resultChanList(s: String) =
      List(Channel(Quote(GString(s))))

    result.get(resultChanList("result0")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result0"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), result0Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result1")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result1"))),
                            ListChannelWithRandom(Seq(Quote(GInt(9))), result1Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result2")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result2"))),
                            ListChannelWithRandom(Seq(Quote(GInt(10))), result2Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result3")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result3"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), result3Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result4")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result4"))),
                            ListChannelWithRandom(Seq(Quote(GInt(9))), result4Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result5")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result5"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), result5Rand),
                            false)),
          List()
        )))
    result.get(resultChanList("result6")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result6"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), result6Rand),
                            false)),
          List()
        )))
    result.get(List[Channel](Quote(Registry.registryRoot))) should be(
      Some(Row(
        List(Datum.create(Channel(Quote(Registry.registryRoot)),
                          ListChannelWithRandom(Seq(Quote(EMapBody(ParMap(SortedParMap.empty)))),
                                                rootRand),
                          false)),
        List()
      )))
  }

  "Public lookup" should "decode and then call lookup" in {
    val lookupString =
      """
      new r(`rho:registry:lookup`) in {
        r!(`rho:id:bnm61w3958nhr5u6wx9yx6c4js77hcxmftc9o1yo4y9yxdu7g8bnq3`, "result0") |
        r!(`rho:id:bnmzm3i5h5hj8qyoh3ubmbu57zuqn56xrk175bw5sf6kook9bq8ny3`, "result1")
      }"""
    val lookupPar: Par                  = Interpreter.buildNormalizedTerm(new StringReader(lookupString)).value
    val completePar                     = lookupPar.addSends(rootSend, branchSend)
    implicit val rand: Blake2b512Random = baseRand.splitByte(4)
    val newRand                         = rand.splitByte(2)
    val randResult0                     = newRand.splitByte(0)
    randResult0.next; randResult0.next
    val randResult1 = newRand.splitByte(1)
    randResult1.next; randResult1.next

    val result = withRegistryAndTestSpace { (reducer, space) =>
      implicit val env = Env[Par]()
      val resultTask = for {
        _ <- reducer.eval(completePar)
      } yield space.store.toMap
      Await.result(resultTask.runAsync, 3.seconds)
    }

    def resultChanList(s: String) =
      List(Channel(Quote(GString(s))))

    result.get(resultChanList("result0")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result0"))),
                            ListChannelWithRandom(Seq(Quote(GInt(8))), randResult0),
                            false)),
          List()
        )))
    result.get(resultChanList("result1")) should be(
      Some(
        Row(
          List(Datum.create(Channel(Quote(GString("result1"))),
                            ListChannelWithRandom(Seq(Quote(GInt(9))), randResult1),
                            false)),
          List()
        )))
  }

  "Random Registry" should "use the random generator and insert" in {
    val registerString                  = """
      new rr(`rho:registry:insertRandom`), rl(`rho:registry:lookup`), x, y in {
        rr!(bundle+{*x}, *y) |
        for(@{uri /\ Uri} <- y) {
          @"result0"!(uri) |
          rl!(uri, "result1")
        }
      }"""
    val registerPar: Par                = Interpreter.buildNormalizedTerm(new StringReader(registerString)).value
    val completePar                     = registerPar.addSends(rootSend, branchSend)
    implicit val rand: Blake2b512Random = baseRand.splitByte(5)
    val newRand                         = rand.splitByte(2)
    val registeredName                  = newRand.next();
    newRand.next()
    val registerRand = newRand.splitByte(0)
    // Once for Uri and twice for temporary channels to handle the insert.
    val uriBytes = registerRand.next();
    registerRand.next(); registerRand.next()
    val insertRand = registerRand
    // Goes directly into root
    insertRand.next();
    val merge0Rand  = Blake2b512Random.merge(Seq(newRand.splitByte(1), insertRand))
    val randResult0 = merge0Rand.splitByte(0)
    val lookupRand  = merge0Rand.splitByte(1)
    lookupRand.next();
    val randResult1 = lookupRand

    val result = withRegistryAndTestSpace { (reducer, space) =>
      implicit val env = Env[Par]()
      val resultTask = for {
        _ <- reducer.eval(completePar)
      } yield space.store.toMap
      Await.result(resultTask.runAsync, 3.seconds)
    }

    def resultChanList(s: String) =
      List(Channel(Quote(GString(s))))

    val expectedBundle: Par =
      Bundle(GPrivate(ByteString.copyFrom(registeredName)), writeFlag = true, readFlag = false)

    val expectedUri = Registry.buildURI(uriBytes)

    // format: off
    result.get(resultChanList("result0")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result0"))),
                       ListChannelWithRandom(Seq(Quote(GUri(expectedUri))), randResult0),
                       false)),
        List()
      )))
    result.get(resultChanList("result1")) should be(
      Some(Row(
        List(
          Datum.create(Channel(Quote(GString("result1"))),
                       ListChannelWithRandom(Seq(Quote(expectedBundle)), randResult1),
                       false)),
        List()
      )))
    // format: on
  }
}
