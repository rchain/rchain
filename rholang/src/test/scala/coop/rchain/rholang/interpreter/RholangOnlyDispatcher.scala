package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import cats.mtl.FunctorTell
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoTuplespace
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.InterpreterError

object RholangOnlyDispatcher {

  def create[M[_], F[_]](tuplespace: RhoTuplespace[M], urnMap: Map[String, Par] = Map.empty)(
      implicit
      cost: _cost[M],
      parallel: Parallel[M],
      s: Sync[M]
  ): (Dispatch[M, ListParWithRandom, TaggedContinuation], DebruijnInterpreter[M]) = {

    lazy val dispatcher: Dispatch[M, ListParWithRandom, TaggedContinuation] =
      new RholangOnlyDispatcher

    implicit lazy val reducer: DebruijnInterpreter[M] =
      new DebruijnInterpreter[M](
        tuplespace,
        dispatcher,
        urnMap
      )

    (dispatcher, reducer)
  }
}

class RholangOnlyDispatcher[M[_]](implicit s: Sync[M], reducer: Reduce[M])
    extends Dispatch[M, ListParWithRandom, TaggedContinuation] {

  def dispatch(
      continuation: TaggedContinuation,
      dataList: Seq[ListParWithRandom]
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
