package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import cats.mtl.FunctorTell
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models.{ListChannelWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.storage.TuplespaceAlg
import coop.rchain.rspace.pure.PureRSpace

object RholangOnlyDispatcher {

  def create[M[_], F[_]](tuplespace: RhoISpace, urnMap: Map[String, Par] = Map.empty)(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]
  ): (Dispatch[M, ListChannelWithRandom, TaggedContinuation], ChargingReducer[M]) = {
    val pureSpace          = PureRSpace[M].of(tuplespace)
    lazy val tuplespaceAlg = TuplespaceAlg.rspaceTuplespace(pureSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, ListChannelWithRandom, TaggedContinuation] =
      new RholangOnlyDispatcher(chargingReducer)
    implicit lazy val costAlg = CostAccountingAlg.unsafe[M](CostAccount(0))
    implicit lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)
    lazy val chargingReducer = new ChargingReducer[M]()
    (dispatcher, chargingReducer)
  }
}

class RholangOnlyDispatcher[M[_]] private (reducer: => ChargingReducer[M])(implicit s: Sync[M])
    extends Dispatch[M, ListChannelWithRandom, TaggedContinuation] {

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListChannelWithRandom]): M[Unit] =
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
