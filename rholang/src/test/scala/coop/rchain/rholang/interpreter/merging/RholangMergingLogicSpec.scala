package coop.rchain.rholang.interpreter.merging

import cats.Applicative
import cats.effect.IO
import cats.syntax.all._
import coop.rchain.shared.scalatestcontrib._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RholangMergingLogicSpec extends AnyFlatSpec with Matchers {

  it should "calculate number channels difference" in effectTest {

    /*
     *        A   B   C        A   B   C
     *  ---------------       ----------
     *  PSH  10      20
     *
     *   0.  20               10
     *   1.       3      ==>       3
     *   2.  15      10       -5     -10
     */

    val chA = "A"
    val chB = "B"
    val chC = "C"

    val initValues = Map(
      (chA, 10L),
      (chC, 20L)
    )

    val input = Seq(
      Map((chA, 20L)),
      Map((chB, 3L)), // Contains change on non-initialized key (channel)
      Map((chA, 15L), (chC, 10L))
    )

    def getDataOnHash[F[_]: Applicative](hash: String): F[Option[Long]] =
      initValues.get(hash).pure[F]

    RholangMergingLogic.calculateNumChannelDiff(input, getDataOnHash[IO]).map { res =>
      res shouldBe Seq(Map(("A", 10)), Map(("B", 3)), Map(("A", -5), ("C", -10)))
    }
  }

}
