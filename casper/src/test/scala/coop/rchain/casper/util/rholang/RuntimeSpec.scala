package coop.rchain.casper.util.rholang

import coop.rchain.casper.syntax._
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRuntimeAt
import coop.rchain.crypto.Blake2b256Hash
import coop.rchain.shared.Log
import coop.rchain.shared.scalatestcontrib.effectTest
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

class RuntimeSpec extends FlatSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global

  "emptyStateHash" should "be the same as hard-coded cached value" in effectTest {
    implicit val log: Log[Task]         = new Log.NOPLog[Task]
    implicit val span: Span[Task]       = new NoopSpan[Task]
    implicit val metrics: Metrics[Task] = new MetricsNOP[Task]

    val kvm = InMemoryStoreManager[Task]()

    for {
      runtimes        <- mkRuntimeAt[Task](kvm)
      (runtime, _, _) = runtimes

      /**
        * Root hashes compatible with RChain main net network
        */
      // Par() - without bootstrap AST
      // 03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314

      // Par(sends = Seq(Send()))
      // 1325a42070be0ac7c44c5c345c5f7512379618d5db57ad76a871d4f34051e05c

      // Par(receives = Seq(Receive(binds = Seq(ReceiveBind()))))
      // 2a5adf05eb519bd0858414e5a4b31a8e22fd64e5203fae4e1ec8f9b1b5113ff0

      hardCodedHash = RuntimeManager.emptyStateHashFixed
      emptyRootHash <- runtime.emptyStateHash

      emptyHashHardCoded = Blake2b256Hash.fromByteString(hardCodedHash)
      emptyHash          = Blake2b256Hash.fromByteString(emptyRootHash)
    } yield emptyHashHardCoded shouldEqual emptyHash
  }
}
