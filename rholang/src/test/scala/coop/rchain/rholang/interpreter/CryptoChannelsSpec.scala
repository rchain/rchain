package coop.rchain.rholang.interpreter

import java.nio.file.Files

import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.{Blake2b256, Blake2b512Random, Keccak256, Sha256}
import coop.rchain.crypto.signatures.{Ed25519, Secp256k1}
import coop.rchain.models.Expr.ExprInstance.{GBool, GByteArray, GString}
import coop.rchain.models.Var.VarInstance.Wildcard
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.serialization.implicits._
import coop.rchain.models.testImplicits._
import coop.rchain.rholang.interpreter.Runtime.RhoIStore
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rspace.Serialize
import coop.rchain.shared.PathOps._
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

  implicit val rand: Blake2b512Random       = Blake2b512Random(Array.empty[Byte])
  implicit val serializePar: Serialize[Par] = storage.implicits.serializePar
  implicit val serializePars: Serialize[ListParWithRandom] =
    storage.implicits.serializePars

  val serialize: Par => Array[Byte]                    = Serialize[Par].encode(_).toArray
  val byteArrayToByteString: Array[Byte] => ByteString = ba => ByteString.copyFrom(ba)
  val byteStringToExpr: ByteString => Expr             = bs => Expr(GByteArray(bs))
  val byteArrayToExpr                                  = byteArrayToByteString andThen byteStringToExpr
  val parToByteString: Par => ByteString               = serialize andThen (ba => ByteString.copyFrom(ba))
  val parToExpr: Par => Expr                           = parToByteString andThen byteStringToExpr

  // this should consume from the `ack` channel effectively preparing tuplespace for next test
  def clearStore(
      store: RhoIStore,
      reduce: ChargingReducer[Task],
      ackChannel: Par,
      timeout: Duration = 3.seconds
  )(implicit env: Env[Par]): Unit = {
    val consume = Receive(
      Seq(ReceiveBind(Seq(EVar(Var(Wildcard(WildcardMsg())))), ackChannel)),
      Par()
    )
    Await.ready(reduce.eval(consume).runToFuture, 3.seconds)
  }

  def assertStoreContains(
      store: RhoIStore
  )(ackChannel: GString)(data: ListParWithRandom): Assertion = {
    val channel: Par = ackChannel
    val datum        = store.toMap(List(channel)).data.head
    assert(datum.a.pars == data.pars)
    assert(datum.a.randomState == data.randomState)
    assert(!datum.persist)
  }

  def hashingChannel(
      channelName: String,
      hashFn: Array[Byte] => Array[Byte],
      fixture: FixtureParam
  ): Any = {
    val (reduce, store) = fixture

    val serializeAndHash: (Array[Byte] => Array[Byte]) => Par => Array[Byte] =
      hashFn => serialize andThen hashFn

    val hashChannel: Par         = GString(channelName)
    val hash: Par => Array[Byte] = serializeAndHash(hashFn)

    val ackChannel        = GString("x")
    implicit val emptyEnv = Env[Par]()

    val storeContainsTest: ListParWithRandom => Assertion =
      assertStoreContains(store)(ackChannel)(_)

    forAll { (par: Par) =>
      val byteArrayToSend = Expr(GByteArray(par.toByteString))
      val data: List[Par] = List(byteArrayToSend, ackChannel)
      val send            = Send(hashChannel, data, false, BitSet())
      val expected        = (hash andThen byteArrayToByteString andThen byteStringToExpr)(par)
      // Send byte array on hash channel. This should:
      // 1. meet with the system process in the tuplespace
      // 2. hash input array
      // 3. send result on supplied ack channel
      Await.result(reduce.eval(send).runToFuture, 3.seconds)
      storeContainsTest(ListParWithRandom(Seq(expected), rand))
      clearStore(store, reduce, ackChannel)
    }
  }

  "sha256Hash channel" should "hash input data and send result on ack channel" in { fixture =>
    hashingChannel("sha256Hash", Sha256.hash _, fixture)
  }

  "blake2b256Hash channel" should "hash input data and send result on ack channel" in { fixture =>
    hashingChannel("blake2b256Hash", Blake2b256.hash _, fixture)
  }

  "keccak256Hash channel" should "hash input data and send result on ack channel" in { fixture =>
    hashingChannel("keccak256Hash", Keccak256.hash _, fixture)

  }

  type Signature  = Array[Byte]
  type PubKey     = Array[Byte]
  type PrivateKey = Array[Byte]
  type Nonce      = Array[Byte]
  type Data       = Array[Byte]

  "secp256k1Verify channel" should "verify integrity of the data and send result on ack channel" in {
    fixture =>
      val (reduce, store) = fixture

      val secp256k1VerifyhashChannel = GString("secp256k1Verify")

      val pubKey = Base16.decode(
        "04C591A8FF19AC9C4E4E5793673B83123437E975285E7B442F4EE2654DFFCA5E2D2103ED494718C697AC9AEBCFD19612E224DB46661011863ED2FC54E71861E2A6"
      )
      val secKey = Base16.decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530")

      val ackChannel        = GString("x")
      implicit val emptyEnv = Env[Par]()
      val storeContainsTest: ListParWithRandom => Assertion =
        assertStoreContains(store)(ackChannel) _

      forAll { (par: Par) =>
        val parByteArray: Array[Byte] = Keccak256.hash(serialize(par))

        val signature = Secp256k1.sign(parByteArray, secKey)

        val serializedPar = byteArrayToExpr(parByteArray)
        val signaturePar  = byteArrayToExpr(signature)
        val pubKeyPar     = byteArrayToExpr(pubKey)

        val refVerify = Secp256k1.verify(parByteArray, signature, pubKey)
        assert(refVerify === true)

        val send = Send(
          secp256k1VerifyhashChannel,
          List(serializedPar, signaturePar, pubKeyPar, ackChannel),
          persistent = false,
          BitSet()
        )
        Await.result(reduce.eval(send).runToFuture, 3.seconds)
        storeContainsTest(
          ListParWithRandom(Seq(Expr(GBool(true))), rand)
        )
        clearStore(store, reduce, ackChannel)
      }
  }

  "ed25519Verify channel" should "verify integrity of the data and send result on ack channel" in {
    fixture =>
      val (reduce, store) = fixture

      implicit val rand = Blake2b512Random(Array.empty[Byte])

      val ed25519VerifyChannel = GString("ed25519Verify")
      val (secKey, pubKey)     = Ed25519.newKeyPair

      val ackChannel        = GString("x")
      implicit val emptyEnv = Env[Par]()
      val storeContainsTest: ListParWithRandom => Assertion =
        assertStoreContains(store)(ackChannel) _

      forAll { (par: Par) =>
        val parByteArray: Array[Byte] = serialize(par)

        val signature = Ed25519.sign(parByteArray, secKey)

        val serializedPar = byteArrayToExpr(parByteArray)
        val signaturePar  = byteArrayToExpr(signature)
        val pubKeyPar     = byteArrayToExpr(pubKey)

        val refVerify = Ed25519.verify(parByteArray, signature, pubKey)
        assert(refVerify === true)

        val send = Send(
          ed25519VerifyChannel,
          List(serializedPar, signaturePar, pubKeyPar, ackChannel),
          persistent = false,
          BitSet()
        )
        Await.result(reduce.eval(send).runToFuture, 3.seconds)
        storeContainsTest(
          ListParWithRandom(List(Expr(GBool(true))), rand)
        )
        clearStore(store, reduce, ackChannel)
      }
  }

  override protected def withFixture(test: OneArgTest): Outcome = {
    val randomInt = scala.util.Random.nextInt
    val dbDir     = Files.createTempDirectory(s"rchain-storage-test-$randomInt")
    val size      = 1024L * 1024 * 10
    val runtime   = Runtime.create(dbDir, size)
    runtime.reducer.setAvailablePhlos(Cost(Integer.MAX_VALUE)).runSyncUnsafe(1.second)

    try {
      test((runtime.reducer, runtime.space.store))
    } finally {
      runtime.close().unsafeRunSync
      dbDir.recursivelyDelete
    }
  }

  /** TODO(mateusz.gorski): once we refactor Rholang[AndScala]Dispatcher
    *  to push effect choice up until declaration site refactor to `Reduce[Coeval]`
    */
  override type FixtureParam = (ChargingReducer[Task], RhoIStore)

}
