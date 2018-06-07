package coop.rchain.rholang.interpreter

import java.nio.file.Files

import com.google.protobuf.ByteString
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.encryption.Curve25519
import coop.rchain.crypto.hash.{Blake2b256, Keccak256, Sha256}
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray, GString}
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

  val serialize: Par => Array[Byte]                    = Serialize[Par].encode _
  val byteArrayToByteString: Array[Byte] => ByteString = ba => ByteString.copyFrom(ba)
  val byteStringToExpr: ByteString => Expr             = bs => Expr(GByteArray(bs))
  val byteArrayToExpr                                  = byteArrayToByteString andThen byteStringToExpr
  val parToByteString: Par => ByteString               = serialize andThen (ba => ByteString.copyFrom(ba))
  val parToExpr: Par => Expr                           = parToByteString andThen byteStringToExpr

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

  def hashingChannel(channelName: String, hashFn: Array[Byte] => Array[Byte]): FixtureParam => Any =
    fixture => {
      val (reduce, store) = fixture

      val serializeAndHash: (Array[Byte] => Array[Byte]) => Par => Array[Byte] =
        hashFn => serialize andThen hashFn

      val hashChannel              = Quote(GString(channelName))
      val hash: Par => Array[Byte] = serializeAndHash(hashFn)

      val ackChannel        = GString("x")
      implicit val emptyEnv = Env[Par]()

      val storeContainsTest: List[Channel] => Assertion = assertStoreContains(store)(ackChannel) _

      forAll { (par: Par) =>
        val byteArrayToSend = Expr(GByteArray(par.toByteString))
        val data: List[Par] = List(byteArrayToSend, ackChannel)
        val send            = Send(hashChannel, data, false, BitSet())
        val expected        = (hash andThen byteArrayToByteString andThen byteStringToExpr)(par)
        // Send byte array on hash channel. This should:
        // 1. meet with the system process in the tuplespace
        // 2. hash input array
        // 3. send result on supplied ack channel
        Await.result(reduce.eval(send).runAsync, 3.seconds)
        storeContainsTest(List[Channel](Quote(expected)))
        clearStore(store, reduce, ackChannel)
      }
    }

  "sha256Hash channel" should "hash input data and send result on ack channel" in { fixture =>
    hashingChannel("sha256Hash", Sha256.hash _)(fixture)

  }

  "blake2b256Hash channel" should "hash input data and send result on ack channel" in { fixture =>
    hashingChannel("blake2b256Hash", Blake2b256.hash _)(fixture)
  }

  "keccak256Hash channel" should "hash input data and send result on ack channel" in { fixture =>
    hashingChannel("keccak256Hash", Keccak256.hash _)(fixture)

  }

  type Signature  = Array[Byte]
  type PubKey     = Array[Byte]
  type PrivateKey = Array[Byte]
  type Nonce      = Array[Byte]
  type Data       = Array[Byte]

  "secp256k1Verify channel" should "verify integrity of the data and send result on ack channel" in {
    fixture =>
      pending
    //TODO: once we have secp256k1 packaged as jar
//      val (reduce, store) = fixture
//
//      val secp256k1VerifyhashChannel = Quote(GString("secp256k1Verify"))
//
//      val (secKey, pubKey) = Secp256k1.newKeyPair
//
//      val ackChannel                                    = GString("x")
//      implicit val emptyEnv                             = Env[Par]()
//      val storeContainsTest: List[Channel] => Assertion = assertStoreContains(store)(ackChannel) _
//
//      forAll { (par: Par) =>
//        val parByteArray: Array[Byte] = serialize(par)
//
//        val signature = Secp256k1.sign(parByteArray, secKey)
//
//        val serializedPar = byteArrayToExpr(parByteArray)
//        val signaturePar  = byteArrayToExpr(signature)
//        val pubKeyPar     = byteArrayToExpr(pubKey)
//
//        val refVerify = Secp256k1.verify(parByteArray, signature, pubKey)
//        assert(refVerify === true)
//
//        val send = Send(secp256k1VerifyhashChannel,
//                        List(serializedPar, signaturePar, pubKeyPar, ackChannel),
//                        persistent = false,
//                        BitSet())
//        Await.result(reduce.eval(send).runAsync, 3.seconds)
//        storeContainsTest(List[Channel](Quote(Expr(GBool(true)))))
//        clearStore(store, reduce, ackChannel)
//      }
  }

  "ed25519Verify channel" should "verify integrity of the data and send result on ack channel" in {
    fixture =>
      val (reduce, store) = fixture

      val ed25519VerifyChannel = Quote(GString("ed25519Verify"))
      val (secKey, pubKey)     = Ed25519.newKeyPair

      val ackChannel                                    = GString("x")
      implicit val emptyEnv                             = Env[Par]()
      val storeContainsTest: List[Channel] => Assertion = assertStoreContains(store)(ackChannel) _

      forAll { (par: Par) =>
        val parByteArray: Array[Byte] = serialize(par)

        val signature = Ed25519.sign(parByteArray, secKey)

        val serializedPar = byteArrayToExpr(parByteArray)
        val signaturePar  = byteArrayToExpr(signature)
        val pubKeyPar     = byteArrayToExpr(pubKey)

        val refVerify = Ed25519.verify(parByteArray, signature, pubKey)
        assert(refVerify === true)

        val send = Send(ed25519VerifyChannel,
                        List(serializedPar, signaturePar, pubKeyPar, ackChannel),
                        persistent = false,
                        BitSet())
        Await.result(reduce.eval(send).runAsync, 3.seconds)
        storeContainsTest(List[Channel](Quote(Expr(GBool(true)))))
        clearStore(store, reduce, ackChannel)
      }
  }

  override protected def withFixture(test: OneArgTest): Outcome = {
    val randomInt = scala.util.Random.nextInt
    val dbDir     = Files.createTempDirectory(s"rchain-storage-test-$randomInt")
    val size      = 1024L * 1024 * 1024 //borrowed from other places in the code
    val runtime   = Runtime.create(dbDir, size)(Capture.taskCapture)

    try {
      test((runtime.reducer, runtime.space.store))
    } finally {
      runtime.close()
      recursivelyDeletePath(dbDir)
    }
  }

  /** TODO(mateusz.gorski): once we refactor Rholang[AndScala]Dispatcher
    *  to push effect choice up until declaration site refactor to `Reduce[Coeval]`
    */
  override type FixtureParam = (Reduce[Task], Store)

}
