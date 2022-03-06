package coop.rchain.catscontrib.laws.discipline

import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws
import Prop._
import cats.{Eq, Monad}
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.catscontrib.laws.MonadTransLaws
import cats.laws.discipline._

trait MonadTransTests[MT[_[_], _]] extends Laws {
  def laws: MonadTransLaws[MT]

  def monadTrans[G[_]: Monad, A: Arbitrary: Eq, B: Eq](
      implicit
      MonadMTG: Monad[MT[G, *]],
      ArbGA: Arbitrary[G[A]],
      ArbGB: Arbitrary[G[B]],
      CogenA: Cogen[A],
      EqMTGA: Eq[MT[G, A]],
      EqMTGB: Eq[MT[G, B]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "monadTrans",
      parent = None,
      "monadTrans identity"    -> forAll(laws.identity[G, A] _),
      "monadTrans composition" -> forAll(laws.composition[G, A, B] _)
    )
}

object MonadTransTests {
  def apply[MT[_[_], _]: MonadTrans]: MonadTransTests[MT] =
    new MonadTransTests[MT] {
      def laws: MonadTransLaws[MT] = MonadTransLaws[MT]
    }
}
