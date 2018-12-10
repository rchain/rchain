package coop.rchain.rspace

import cats.Id
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringsCaptor, Wildcard}
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.internal._

trait JoinOperationsTests
    extends StorageTestsBase[Id, String, Pattern, Nothing, String, StringsCaptor] {

  "joins" should "remove joins if no PsK" in withTestSpace { space =>
    val store = space.store

    store.withTxn(store.createTxnWrite()) { txn =>
      store.putDatum(txn, List("ch1"), Datum.create("ch1", "datum1", persist = false))
      store.putDatum(txn, List("ch2"), Datum.create("ch2", "datum2", persist = false))
      store.addJoin(txn, "ch1", List("ch1", "ch2"))
      store.addJoin(txn, "ch2", List("ch1", "ch2"))

      //ensure that doubled addJoin creates only one entry
      store.addJoin(txn, "ch1", List("ch1", "ch2"))
      store.addJoin(txn, "ch2", List("ch1", "ch2"))
    }

    store.withTxn(store.createTxnRead()) { txn =>
      store.getJoin(txn, "ch1") shouldBe List(List("ch1", "ch2"))
      store.getJoin(txn, "ch2") shouldBe List(List("ch1", "ch2"))
    }

    store.withTxn(store.createTxnWrite()) { txn =>
      store.removeJoin(txn, "ch1", List("ch1", "ch2"))
      store.removeJoin(txn, "ch2", List("ch1", "ch2"))
    }

    store.withTxn(store.createTxnRead()) { txn =>
      store.getJoin(txn, "ch1") shouldBe List.empty[List[String]]
      store.getJoin(txn, "ch2") shouldBe List.empty[List[String]]
    }

    store.isEmpty shouldBe false

    //now ensure that garbage-collection works and all joins
    //are removed when we remove As
    store.withTxn(store.createTxnWrite()) { txn =>
      store.removeDatum(txn, List("ch1"), 0)
      store.removeDatum(txn, List("ch2"), 0)
    }

    store.isEmpty shouldBe true
  }

}
