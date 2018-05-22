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

  "putDatum" should "put datum" in withTestStore { store =>
    forAll("channel", "datum") { (channel: String, datumValue: String) =>
      val key   = List(channel)
      val datum = Datum(datumValue, persist = false)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putDatum(txn, key, datum)
        store.getData(txn, key) should contain theSameElementsAs (Seq(datum))
        store.removeAll(txn, key)
      }
    }
  }

  it should "append datum if channel already exists" in withTestStore { store =>
    forAll("channel", "datum") { (channel: String, datumValue: String) =>
      val key    = List(channel)
      val datum1 = Datum(datumValue, persist = false)
      val datum2 = Datum(datumValue + "2", persist = false)

      store.withTxn(store.createTxnWrite()) { txn =>
        store.putDatum(txn, key, datum1)
        store.putDatum(txn, key, datum2)
        store.getData(txn, key) should contain theSameElementsAs (Seq(datum1, datum2))
        store.removeAll(txn, key)
      }
    }
  }

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
