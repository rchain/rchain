//package coop.rchain.casper.helper
//
//import coop.rchain.casper.Estimator
//import coop.rchain.metrics.{Metrics, NoopSpan, Span}
//import coop.rchain.shared.Log
//import monix.eval.Task
//
//trait UnlimitedParentsEstimatorFixture {
//  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP[Task]()
//  implicit val span: Span[Task]       = NoopSpan[Task]()
//  implicit val log: Log[Task]         = new Log.NOPLog[Task]()
//  implicit val estimator              = Estimator[Task](Estimator.UnlimitedParents, None)
//}
