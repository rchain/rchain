package coop.rchain.rholang.interpreter

import java.nio.file.Files

import coop.rchain.crypto.hash.Sha256
import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance.{GByteArray, GString}
import coop.rchain.models.Var.VarInstance.Wildcard
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.models.implicits._
import coop.rchain.models.testImplicits._
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.rspace.internal.{Datum, Row}
import coop.rchain.rspace.test._
import coop.rchain.rspace.{IStore, Serialize}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.prop.PropertyChecks
import org.scalatest.{fixture, Assertion, Matchers, Outcome}

import scala.collection.immutable.BitSet
import scala.concurrent.Await
import scala.concurrent.duration._

class CryptoChannelsSpec
    extends fixture.FlatSpec
    with PropertyChecks
    with Matchers
    with TripleEqualsSupport {
  behavior of "Crypto channels"

  type Store = IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation]

  val serialize: Par => Array[Byte] = Serialize[Par].encode _
  val serializeAndHash: (Array[Byte] => Array[Byte]) => Par => Array[Byte] =
    hashFn => serialize andThen hashFn

  // this should consume from the `ack` channel effectively preparing tuplespace for next test
  def clearStore(store: Store,
                 reduce: Reduce[Task],
                 ackChannel: Par,
                 timeout: Duration = 3.seconds)(implicit env: Env[Par]): Unit = {
    val consume = Receive(
      Seq(ReceiveBind(Seq(Channel(ChanVar(Var(Wildcard(WildcardMsg()))))), Quote(ackChannel))),
      Par()
    )
    Await.ready(reduce.eval(consume).runAsync, 3.seconds)
  }

  def assertStoreContains(store: Store)(ackChannel: GString)(data: List[Channel]): Assertion =
    store.toMap(List(Channel(Quote(ackChannel)))) should be(
      Row(
        List(Datum[List[Channel]](data, false)),
        List()
      )
    )

  "sha256Hash channel" should "hash input data and send result on ack channel" in { fixture =>
    val (reduce, store) = fixture

    val Sha256HashChannel              = Quote(GString("sha256Hash"))
    val sha256Hash: Par => Array[Byte] = serializeAndHash(Sha256.hash)

    val ackChannel        = GString("x")
    implicit val emptyEnv = Env[Par]()

    val storeContainsTest: List[Channel] => Assertion = assertStoreContains(store)(ackChannel) _

    forAll { (par: Par) =>
      val byteArrayToSend = Expr(GByteArray(par.toByteString))
      val data: List[Par] = List(byteArrayToSend, ackChannel)
      val send            = Send(Sha256HashChannel, data, false, BitSet())
      val expected        = Expr(GByteArray(com.google.protobuf.ByteString.copyFrom(sha256Hash(par))))
      // Send byte array on sha256Hash channel. This should:
      // 1. meet with the system process in the tuplespace
      // 2. hash input array
      // 3. send result on supplied ack channel
      Await.ready(reduce.eval(send).runAsync, 3.seconds)
      storeContainsTest(List[Channel](Quote(expected)))
      clearStore(store, reduce, ackChannel)
    }
  }

  type Signature = Array[Byte]
  type PubKey = Array[Byte]

  "ed25519Verify" should "verify integrity of the data" in { fixture =>
    val (reduce, store) = fixture

    val ed25519VerifyChannel = Quote(GString("ed25519Verify"))
    val ed25519Verify: (Par, Signature, PubKey) => Boolean = ???

  }

  override protected def withFixture(test: OneArgTest): Outcome = {
    val randomInt = scala.util.Random.nextInt
    val dbDir     = Files.createTempDirectory(s"rchain-storage-test-$randomInt")
    val size      = 1024 * 1024 * 1024 //borrowed from other places in the code
    val runtime   = Runtime.create(dbDir, size)

    try {
      test((runtime.reducer, runtime.store))
    } finally {
      runtime.store.close()
      recursivelyDeletePath(dbDir)
    }
  }

  /** TODO(mateusz.gorski): once we refactor Rholang[AndScala]Dispatcher
    *  to push effect choice up until declaration site refactor to `Reduce[Coeval]`
    */
  override type FixtureParam = (Reduce[Task], Store)

}
