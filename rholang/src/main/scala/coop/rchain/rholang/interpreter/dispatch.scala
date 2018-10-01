package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.mtl.FunctorTell
import cats.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import cats.implicits._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.storage.implicits._
import coop.rchain.rholang.interpreter.storage.{ChargingRSpace, TuplespaceAlg}
import coop.rchain.rspace.pure.PureRSpace
trait Dispatch[M[_], A, K] {
  def dispatch(continuation: K, dataList: Seq[A]): M[Unit]
}

object Dispatch {

  // TODO: Make this function total
  def buildEnv(dataList: Seq[ListParWithRandom]): Env[Par] =
    Env.makeEnv(dataList.flatMap(_.pars): _*)
}

class RholangAndScalaDispatcher[M[_]] private (
    reducer: => ChargingReducer[M],
    _dispatchTable: => Map[Long, Function1[Seq[ListParWithRandom], M[Unit]]]
)(implicit s: Sync[M])
    extends Dispatch[M, ListParWithRandom, TaggedContinuation] {

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListParWithRandom]): M[Unit] =
    for {
      res <- continuation.taggedCont match {
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
    } yield res

}

object RholangAndScalaDispatcher {

  def create[M[_], F[_]](
      tuplespace: RhoISpace,
      dispatchTable: => Map[Long, Function1[Seq[ListParWithRandom], M[Unit]]],
      urnMap: Map[String, Par]
  )(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]
  ): (Dispatch[M, ListParWithRandom, TaggedContinuation], ChargingReducer[M], Registry[M]) = {
    lazy val chargingRSpace = ChargingRSpace.pureRSpace(s, costAlg, tuplespace)
    lazy val tuplespaceAlg  = TuplespaceAlg.rspaceTuplespace(chargingRSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, ListParWithRandom, TaggedContinuation] =
      new RholangAndScalaDispatcher(chargingReducer, dispatchTable)
    implicit lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)
    implicit lazy val costAlg      = CostAccountingAlg.unsafe[M](CostAccount(0))
    lazy val chargingReducer       = new ChargingReducer[M]
    lazy val registry: Registry[M] = new RegistryImpl(chargingRSpace, dispatcher)
    (dispatcher, chargingReducer, registry)
  }
}
