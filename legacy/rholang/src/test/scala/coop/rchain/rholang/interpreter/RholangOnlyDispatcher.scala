package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.RhoRuntime.RhoTuplespace
import coop.rchain.rholang.interpreter.accounting._

object RholangOnlyDispatcher {

  def apply[F[_]: Sync: Parallel: _cost](
      tuplespace: RhoTuplespace[F],
      urnMap: Map[String, Par],
      mergeChs: Ref[F, Set[Par]]
  ): (Dispatch[F, ListParWithRandom, TaggedContinuation], DebruijnInterpreter[F]) = {

    lazy val dispatcher: Dispatch[F, ListParWithRandom, TaggedContinuation] =
      new RholangOnlyDispatcher

    implicit lazy val reducer: DebruijnInterpreter[F] =
      new DebruijnInterpreter[F](tuplespace, dispatcher, urnMap, mergeChs, Par())

    (dispatcher, reducer)
  }

  def apply[F[_]: Sync: Parallel: _cost](
      tuplespace: RhoTuplespace[F],
      urnMap: Map[String, Par] = Map.empty
  ): (Dispatch[F, ListParWithRandom, TaggedContinuation], DebruijnInterpreter[F]) = {
    val initMergeChannelsRef = Ref.unsafe[F, Set[Par]](Set.empty)

    apply(tuplespace, urnMap, initMergeChannelsRef)
  }
}

class RholangOnlyDispatcher[M[_]](implicit s: Sync[M], reducer: Reduce[M])
    extends Dispatch[M, ListParWithRandom, TaggedContinuation] {

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListParWithRandom]): M[Unit] =
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
