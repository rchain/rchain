import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.v2.casper.syntax.all._
import coop.rchain.casper.v2.core.SafetyOracle
import coop.rchain.v2.casper.{DependencyGraph, SafetyOracle}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class SafetyOracleSpec extends FlatSpec with Matchers {

  type Validator = Int
  type Message   = Int
  val bonds: Map[Validator, Long] = Map(1 -> 1L, 2 -> 1L, 3 -> 1L)

  // format: off
  val justificationsMap: Map[Message, List[Message]] = Map(
    21 -> List(0, 12, 13),  22 -> List(0, 12, 13),
                            12 -> List(0, 0, 0),    13 -> List(0, 0, 0),
    0 -> List()
  )
  val sendersMap: Map[Message, Validator] = Map(
    21 -> 1,                22 -> 2,
                            12 -> 2,                13 -> 3,
    0 -> 1
  )
  val seqNumsMap: Map[Message, Int] = Map(
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
      override def sender(message: Message): Validator = sendersMap(message)
      override def seqNum(message: Message): Int       = seqNumsMap(message)
    }

  def oracle[F[_]: Sync](compat: Compatibility): SafetyOracle[F, Message, Validator] =
    new SafetyOracle[F, Message, Validator] {
      override def bondsMap(message: Message): Map[Validator, Long] = bonds
      override def compatible(source: Message, target: Message): Boolean =
        compat.compatible(source, target)
    }
  implicit val s = Sync[Task]

  // Oracle that does not account compatibility and just uses justifications to propagate agreements
  val o = oracle[Task](AlwaysCompatible)
  val d = dag[Task]

  val fts = List(
    List((13, -1.0), (21, -1.0), (22, -1.0)),
    List((0, 1.0), (12, 1.0), (13, 1.0)),
    List((0, 1.0))
  )
  "faultTolerances" should "output correct fault tolerances" in {
    val output = o
      .faultTolerances(Set(21, 22, 13), d)
      .compile
      .toList
      .runSyncUnsafe()
    output shouldBe fts
  }

  "faultTolerances" should "output correct fault tolerances under incompatibility" in {
    // Setup is the same as in previous tests but 31 and 22 are not compatible
    val o = oracle[Task]((l: Message, r: Message) => if (l == 21 && r == 12) false else true)
    val output = o
      .faultTolerances(Set(21, 22, 13), d)
      .compile
      .toList
      .runSyncUnsafe()
    // As 21 is incompatible with 12, 21 won't propagate agreement and only 12 agrees on itself, so 12 is not finalized.
    output.flatten.find { case (m, _) => m == 12 }.get._2 shouldBe -1
  }
}
