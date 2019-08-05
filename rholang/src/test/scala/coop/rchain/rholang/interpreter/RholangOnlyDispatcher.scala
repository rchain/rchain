package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import cats.mtl.FunctorTell
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.Span
import coop.rchain.metrics.Span.TraceId
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rspace.pure.PureRSpace

object RholangOnlyDispatcher {

  def create[M[_], F[_]](tuplespace: RhoISpace[M], urnMap: Map[String, Par] = Map.empty)(
      implicit
      cost: _cost[M],
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable],
      spanM: Span[M]
  ): (Dispatch[M, ListParWithRandom, TaggedContinuation], ChargingReducer[M]) = {

    val pureSpace = PureRSpace[M].of(tuplespace)(matchListPar)

    lazy val dispatcher: Dispatch[M, ListParWithRandom, TaggedContinuation] =
      new RholangOnlyDispatcher

    implicit lazy val reducer: Reduce[M] =
      new DebruijnInterpreter[M, F](
        pureSpace,
        dispatcher,
        urnMap
      )

    val chargingReducer: ChargingReducer[M] = ChargingReducer[M]

    (dispatcher, chargingReducer)
  }
}

class RholangOnlyDispatcher[M[_]](implicit s: Sync[M], chargingReducer: ChargingReducer[M])
    extends Dispatch[M, ListParWithRandom, TaggedContinuation] {

  def dispatch(
      continuation: TaggedContinuation,
      dataList: Seq[ListParWithRandom],
      sequenceNumber: Int
  )(traceId: TraceId): M[Unit] =
    for {
      res <- continuation.taggedCont match {
              case ParBody(parWithRand) =>
                val env     = Dispatch.buildEnv(dataList)
                val randoms = parWithRand.randomState +: dataList.toVector.map(_.randomState)
                chargingReducer.eval(parWithRand.body)(
                  env,
                  Blake2b512Random.merge(randoms),
                  0,
                  traceId
                )
              case ScalaBodyRef(_) =>
                s.unit
              case Empty =>
                s.unit
            }
    } yield res
}
