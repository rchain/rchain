package coop.rchain.rspace

import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.extended._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test._
import org.scalactic.anyvals.PosInt
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait IStoreTests
    extends StorageTestsBase[String, Pattern, String, StringsCaptor]
    with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(1000))

  "collectGarbage" should "not remove used channels" in withTestStore { store =>
    forAll("channel", "datum") { (channel: String, datum: String) =>
      val key  = List(channel)
      val hash = store.hashChannels(key)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putDatum(txn, key, Datum(datum, persist = false))

        store.collectGarbage(txn, store.hashChannels(key))
        store.getChannels(txn, hash) should contain theSameElementsAs (key)
      }
    }
  }

  it should "remove obsolete channels" in withTestStore { store =>
    forAll("channel", "datum") { (channel: String, datum: String) =>
      val key  = List(channel)
      val hash = store.hashChannels(key)
      store.withTxn(store.createTxnWrite()) { txn =>
        store.putDatum(txn, key, Datum(datum, persist = false))
        // collectGarbage is called in removeDatum:
        store.removeDatum(txn, key, 0)
        store.getChannels(txn, hash) shouldBe empty
      }
    }
  }
}
