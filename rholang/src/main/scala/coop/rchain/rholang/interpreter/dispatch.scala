package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.{Ref, Sync}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.CostAccounting.CostStateRef
import coop.rchain.rholang.interpreter.RhoRuntime.RhoTuplespace

trait Dispatch[M[_], A, K] {
  def dispatch(continuation: K, dataList: Seq[A]): M[Unit]
}

object Dispatch {
  def buildEnv(dataList: Seq[ListParWithRandom]): Env[Par] =
    Env.makeEnv(dataList.flatMap(_.pars): _*)
}

class RholangAndScalaDispatcher[M[_]] private (
    _dispatchTable: => Map[Long, Seq[ListParWithRandom] => M[Unit]]
)(implicit s: Sync[M], reducer: Reduce[M])
    extends Dispatch[M, ListParWithRandom, TaggedContinuation] {

  def dispatch(
      continuation: TaggedContinuation,
      dataList: Seq[ListParWithRandom]
  ): M[Unit] =
    continuation.taggedCont match {
      case ParBody(parWithRand) =>
        val env     = Dispatch.buildEnv(dataList)
        val randoms = parWithRand.randomState +: dataList.toVector.map(_.randomState)
        reducer.eval(parWithRand.body)(env, Blake2b512Random.merge(randoms))
      case ScalaBodyRef(ref) =>
        _dispatchTable.get(ref) match {
          case Some(f) => f(dataList)
          case None    => s.raiseError(new Exception(s"dispatch: no function for $ref"))
        }
      case Empty =>
        s.unit
    }
}

object RholangAndScalaDispatcher {
  type RhoDispatch[F[_]] = Dispatch[F, ListParWithRandom, TaggedContinuation]

  def apply[M[_]: Sync: Parallel: CostStateRef](
      tuplespace: RhoTuplespace[M],
      dispatchTable: => Map[Long, Seq[ListParWithRandom] => M[Unit]],
      urnMap: Map[String, Par],
      mergeChs: Ref[M, Set[Par]],
      mergeableTagName: Par
  ): (Dispatch[M, ListParWithRandom, TaggedContinuation], Reduce[M]) = {

    implicit lazy val dispatcher: Dispatch[M, ListParWithRandom, TaggedContinuation] =
      new RholangAndScalaDispatcher(dispatchTable)

    implicit lazy val reducer: Reduce[M] =
      new DebruijnInterpreter[M](tuplespace, dispatcher, urnMap, mergeChs, mergeableTagName)

    (dispatcher, reducer)
  }

}
