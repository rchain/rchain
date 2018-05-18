package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.utils.PersistentStoreTester
import org.scalatest.FlatSpec
import coop.rchain.crypto.hash.Sha256
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models._
import coop.rchain.models.Expr.ExprInstance.{ByteArray, GInt}
import coop.rchain.rspace.{IStore, Serialize}
import implicits._
import coop.rchain.models.implicits._
import monix.eval.Task
import org.scalactic.TripleEqualsSupport

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import scalapb.descriptors.ScalaType.ByteString

import scala.concurrent.Await

class CryptoChannelsSpec extends FlatSpec with TripleEqualsSupport with PersistentStoreTester {
  behavior of "Crypto channels"

  type Store = IStore[Channel, BindPattern, Seq[Channel], TaggedContinuation]

//  def assertStoreContains(store: Store, expected: ???): Boolean = ???

  def sendOnChannel(data: Seq[Seq[Channel]]): Task[Unit] = ???

  val serialize: Par => Array[Byte] = Serialize[Par].encode _
  val serializeAndHash: (Array[Byte] => Array[Byte]) => Par => Array[Byte] =
    hashFn => serialize andThen hashFn

  "sha256Hash channel" should "hash input data and send result on ack channel" in {
    val par: Par = EPlus(GInt(7), GInt(8))
    val input: Seq[Seq[Channel]] = Seq(Seq(Channel(Quote(Expr(ByteArray(par.toByteString))))))
    val sha256Hash: Par => Array[Byte] = serializeAndHash(Sha256.hash)
    val expected = com.google.protobuf.ByteString.copyFrom(sha256Hash(par))
    val storeState = withTestStore { testStore =>
      val task = SystemProcesses.sha256Hash(testStore, ???)(input)
        .map(_ => testStore.toMap)
      Await.result(task.runAsync, 3.seconds)
    }
    //TODO: test that hash was sent on ack channel
    //TODO: test that sent hash is equal to hashing the `expected`
  }
}
