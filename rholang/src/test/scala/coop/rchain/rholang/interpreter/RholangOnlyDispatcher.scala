package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import cats.mtl.FunctorTell
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.storage.Tuplespace
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.Match
import coop.rchain.rspace.pure.PureRSpace

object RholangOnlyDispatcher {

  def create[M[_], F[_]](tuplespace: RhoISpace[M], urnMap: Map[String, Par] = Map.empty)(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]
  ): (Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation], ChargingReducer[M]) = {

    implicit val matchCost: Match[
      M,
      BindPattern,
      errors.InterpreterError,
      ListParWithRandom,
      ListParWithRandomAndPhlos
    ] = matchListPar(Cost(Integer.MAX_VALUE))

    val pureSpace = PureRSpace[M].of(tuplespace)

    implicit val costAlg: CostAccounting[M] = CostAccounting.unsafe[M](Cost(0))
    implicit val cost: _cost[M]             = costAlg

    lazy val dispatcher: Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation] =
      new RholangOnlyDispatcher

    lazy val tuplespaceAlg = Tuplespace.rspaceTuplespace(pureSpace, dispatcher)

    implicit lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)

    val chargingReducer: ChargingReducer[M] = ChargingReducer[M]

    (dispatcher, chargingReducer)
  }
}

class RholangOnlyDispatcher[M[_]](implicit s: Sync[M], chargingReducer: ChargingReducer[M])
    extends Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation] {

  def dispatch(
      continuation: TaggedContinuation,
      dataList: Seq[ListParWithRandomAndPhlos],
      sequenceNumber: Int
  ): M[Unit] =
    for {
      res <- continuation.taggedCont match {
              case ParBody(parWithRand) =>
                val env     = Dispatch.buildEnv(dataList)
                val randoms = parWithRand.randomState +: dataList.toVector.map(_.randomState)
                chargingReducer.eval(parWithRand.body)(env, Blake2b512Random.merge(randoms))
              case ScalaBodyRef(_) =>
                s.unit
              case Empty =>
                s.unit
            }
    } yield res
}
