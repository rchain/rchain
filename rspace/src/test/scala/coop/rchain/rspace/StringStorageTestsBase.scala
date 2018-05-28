package coop.rchain.rspace

import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.test.{ImmutableInMemStore, InMemoryStore}

class InMemoryStoreTestsBase extends InMemStorageTestBase[String, Pattern, String, StringsCaptor] {
  override def createTestStore
    : IStore[String, Pattern, String, StringsCaptor] with ITestableStore[String, Pattern] =
    InMemoryStore.create[String, Pattern, String, StringsCaptor]
}

class ImmutableInMemoryStoreTestsBase
    extends InMemStorageTestBase[String, Pattern, String, StringsCaptor] {
  override def createTestStore
    : IStore[String, Pattern, String, StringsCaptor] with ITestableStore[String, Pattern] =
    ImmutableInMemStore.create[String, Pattern, String, StringsCaptor]
}

class StringLMDBStoreTestsBase extends LMDBStoreTestsBase[String, Pattern, String, StringsCaptor] {}
