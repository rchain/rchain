import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.v2.casper.Casper.{ConflictScope, Finalize, MessageScope}
import coop.rchain.v2.casper.syntax.all._
import coop.rchain.casper.v2.core.SafetyOracle
import coop.rchain.v2.casper.{DependencyGraph, SafetyOracle}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import fs2.Stream

class CasperSpec extends FlatSpec with Matchers {

  type Validator = Int
  type Message   = Int
  val bonds: Map[Validator, Long] = Map(1 -> 1L, 2 -> 1L, 3 -> 1L)

  // format: off
  val justificationsMap: Map[Message, List[Message]] = Map(
                            32 -> List(21, 22, 13), 33 -> List(21, 22, 13),
    21 -> List(0, 12, 13),  22 -> List(0, 12, 13),
                            12 -> List(0, 0, 0),    13 -> List(0, 0, 0),
    0 -> List()
  )
  val sendersMap: Map[Message, Validator] = Map(
                            32 -> 2,                33 -> 3,
    21 -> 1,                22 -> 2,
                            12 -> 2,                13 -> 3,
    0 -> 1
  )
  val seqNumsMap: Map[Message, Int] = Map(
                            32 -> 3,                33 -> 3,
    21 -> 2,                22 -> 2,
                            12 -> 1,                13 -> 1,
    0 -> 0
  )
  // format: on

  trait Compatibility { def compatible(l: Message, r: Message): Boolean }
  case object AlwaysCompatible extends Compatibility {
    override def compatible(l: Message, r: Message): Boolean = true
  }

  def dag[F[_]: Sync]: DependencyGraph[F, Message, Validator] =
    new DependencyGraph[F, Message, Validator] {
      override def justifications(message: Message): F[List[Message]] =
        justificationsMap(message).pure[F]
      override def sender(message: Message): Validator                = sendersMap(message)
      override def seqNum(message: Message): Int                      = seqNumsMap(message)
    }

  def oracle[F[_]: Sync](compat: Compatibility): SafetyOracle[F, Message, Validator] =
    new SafetyOracle[F, Message, Validator] {
      override def bondsMap(message: Message): Map[Validator, Long]      = bonds
      override def compatible(source: Message, target: Message): Boolean =
        compat.compatible(source, target)
    }

  def casper[F[_]: Sync]: Casper[F, Message, Validator] = new Casper[F, Message, Validator] {
    override def faultToleranceThreshold = -1
    override val maxDepth: Long          = 10000
  }
  implicit val s                                        = Sync[Task]
  val d                                                 = dag[Task]
  val o                                                 = oracle[Task](AlwaysCompatible)
  val c                                                 = casper[Task]

  "Message scope" should "be correct" in {
    val messageScope = c
      .casperScope(Set(21, 32, 33), d, o)
      .runSyncUnsafe()
    messageScope shouldBe MessageScope(
      FinalizationFringe(Set(21, 22, 13)),
      ConflictScope(Set(32, 33))
    )
  }
  "Message scope on truncated DAG" should "be correct" in {
    val messageScope = c
      .casperScope(Set(21, 22, 13), d, o)
      .runSyncUnsafe()
    messageScope shouldBe MessageScope(
      FinalizationFringe(Set(0, 12, 13)),
      ConflictScope(Set(21, 22))
    )
  }

  "Finalization fringe" should "contain highest finalized messages" in {
    val d1 = new DependencyGraph[Task, Message, Validator] {
      override def justifications(message: Message): Task[List[Message]] =
        if (message == 32) List(0, 22, 13).pure[Task] else d.justifications(message)
      override def sender(message: Message): Validator                   = d.sender(message)
      override def seqNum(message: Message): Int                         = d.seqNum(message)
    }

    // Ordinary DAG outputs 21 as the first finalized
    o.faultTolerances(Set(21, 32, 33), d)
      .flatMap(Stream.emits)
      .filter { case (m, _) => d1.sender(m) == 1 }
      .compile
      .toList
      .runSyncUnsafe() shouldBe List((21, -1.0), (0, -1.0), (21, 1.0), (0, 1.0), (0, 1.0))

    // Change to justification makes for validator 1 message 0 first occur as a finalized, before 21 is visited.
    // But still, 21 should be part of the fringe, not 0.
    o.faultTolerances(Set(21, 32, 33), d1)
      .flatMap(Stream.emits)
      .filter { case (m, _) => d1.sender(m) == 1 }
      .compile
      .toList
      .runSyncUnsafe() shouldBe List((21, -1.0), (0, 1.0), (21, 1.0), (0, 1.0), (0, 1.0))
    val messageScope = c
      .casperScope(Set(21, 32, 33), d1, o)
      .runSyncUnsafe()
    messageScope shouldBe MessageScope(
      FinalizationFringe(Set(21, 22, 13)),
      ConflictScope(Set(32, 33))
    )
  }
}
