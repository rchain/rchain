package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import cats.mtl.FunctorTell
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models.{
  ListChannelWithRandom,
  ListChannelWithRandomAndPhlos,
  Par,
  TaggedContinuation
}
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.storage.TuplespaceAlg
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.pure.PureRSpace

object RholangOnlyDispatcher {

  def create[M[_], F[_]](tuplespace: RhoISpace, urnMap: Map[String, Par] = Map.empty)(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]
  ): (Dispatch[M, ListChannelWithRandomAndPhlos, TaggedContinuation], ChargingReducer[M]) = {
    // This is safe because test
    implicit val matchCost = matchListQuote(Cost(Integer.MAX_VALUE))
    val pureSpace          = PureRSpace[M].of(tuplespace)
    lazy val tuplespaceAlg = TuplespaceAlg.rspaceTuplespace(pureSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, ListChannelWithRandomAndPhlos, TaggedContinuation] =
      new RholangOnlyDispatcher(chargingReducer)
    implicit lazy val costAlg = CostAccountingAlg.unsafe[M](CostAccount(0))
    implicit lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)
    lazy val chargingReducer = new ChargingReducer[M]()
    (dispatcher, chargingReducer)
  }
}

class RholangOnlyDispatcher[M[_]] private (reducer: => ChargingReducer[M])(implicit s: Sync[M])
    extends Dispatch[M, ListChannelWithRandomAndPhlos, TaggedContinuation] {

  def dispatch(
      continuation: TaggedContinuation,
      dataList: Seq[ListChannelWithRandomAndPhlos]
  ): M[Unit] =
    for {
      res <- continuation.taggedCont match {
              case ParBody(parWithRand) =>
                val env     = Dispatch.buildEnv(dataList)
                val randoms = parWithRand.randomState +: dataList.toVector.map(_.randomState)
                reducer.eval(parWithRand.body)(env, Blake2b512Random.merge(randoms))
              case ScalaBodyRef(_) =>
                s.unit
              case Empty =>
                s.unit
            }
    } yield res
}
