package coop.rchain.casper.rholang

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.syntax._
import coop.rchain.models.syntax._
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import coop.rchain.shared.RChainScheduler._

// TODO enable when CE is migrated to 3 (cats.effect.testing.scalatest is not available for CE2)
//class RuntimeSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
//
//  "emptyStateHash" should "be the same as hard-coded cached value" in {
//    implicit val log: Log[IO]         = new Log.NOPLog[IO]
//    implicit val span: Span[IO]       = new NoopSpan[IO]
//    implicit val metrics: Metrics[IO] = new MetricsNOP[IO]
//
//    val kvm = InMemoryStoreManager[IO]()
//
//    val dummyShardId = "dummy"
//    for {
//      store <- kvm.rSpaceStores
//      runtime <- RhoRuntime.createRuntime(
//                  store,
//                  BlockRandomSeed.nonNegativeMergeableTagName(dummyShardId),
//                  rholangEC
//                )
//
//      /**
//        * Root hashes compatible with RChain main net network
//        */
//      // Par() - without bootstrap AST
//      // 03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314
//
//      // Par(sends = Seq(Send()))
//      // 1325a42070be0ac7c44c5c345c5f7512379618d5db57ad76a871d4f34051e05c
//
//      // Par(receives = Seq(Receive(binds = Seq(ReceiveBind()))))
//      // 2a5adf05eb519bd0858414e5a4b31a8e22fd64e5203fae4e1ec8f9b1b5113ff0
//
//      hardCodedHash = RuntimeManager.emptyStateHashFixed
//      emptyRootHash <- runtime.emptyStateHash
//
//      emptyHashHardCoded = hardCodedHash.toBlake2b256Hash
//      emptyHash          = emptyRootHash.toBlake2b256Hash
//    } yield emptyHashHardCoded shouldEqual emptyHash
//  }
//
//  "stateHash after fixed rholang term execution " should "be hash fixed without hard fork" in {
//    implicit val metricsEff: Metrics[IO] = new Metrics.MetricsNOP[IO]
//    implicit val noopSpan: Span[IO]      = NoopSpan[IO]()
//    implicit val logger: Log[IO]         = Log.log[IO]
//    val kvm                              = InMemoryStoreManager[IO]()
//    val dummyShardId                     = "dummy"
//
//    // fixed term , if the term changed, it is possible that the stateHash also changed.
//    val contract =
//      """
//        | new a in {
//        |   @"2"!(10)|
//        |   @2!("test")|
//        |   @"3"!!(3)|
//        |   @42!!("1")|
//        |   for (@t <- a){Nil}|
//        |   for (@num <- @"3"&@num2 <- @1){10}|
//        |   for (@_ <= @"4"){"3"}|
//        |   for (@_ <= @"5"& @num3 <= @5){Nil}|
//        |   for (@3 <- @44){new g in {Nil}}|
//        |   for (@_ <- @"55"& @num3 <- @55){Nil}
//        | }
//        |""".stripMargin
//
//    // random seed should be always to the same to make sure everything is the same
//    val random =
//      Tools.rng(Blake2b256Hash.create(Array[Byte](1)).toByteString.toByteArray)
//
//    for {
//      store <- kvm.rSpaceStores
//      runtime <- RhoRuntime.createRuntime(
//                  store,
//                  BlockRandomSeed.nonNegativeMergeableTagName(dummyShardId),
//                  rholangEC
//                )
//      r          <- runtime.evaluate(contract, Cost.UNSAFE_MAX, Map.empty, random)
//      _          = r.errors should be(Vector.empty)
//      checkpoint <- runtime.createCheckpoint
//      expectedHash = Blake2b256Hash.fromHex(
//        "10cce029738696f1e120a6bad4bdf3f18adca25ccf36133bd4916f607a6a50c0"
//      )
//      stateHash = checkpoint.root
//    } yield expectedHash shouldEqual stateHash
//  }
//
//}
