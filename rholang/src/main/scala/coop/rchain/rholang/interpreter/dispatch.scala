package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.mtl.FunctorTell
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import cats.implicits._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccounting}
import coop.rchain.rholang.interpreter.storage.{ChargingRSpace, Tuplespace}
trait Dispatch[M[_], A, K] {
  def dispatch(continuation: K, dataList: Seq[A], sequenceNumber: Int): M[Unit]
}

object Dispatch {

  // TODO: Make this function total
  def buildEnv(dataList: Seq[ListParWithRandomAndPhlos]): Env[Par] =
    Env.makeEnv(dataList.flatMap(_.pars): _*)
}

class RholangAndScalaDispatcher[M[_]] private (
    reducer: => ChargingReducer[M],
    _dispatchTable: => Map[Long, (Seq[ListParWithRandomAndPhlos], Int) => M[Unit]]
)(implicit s: Sync[M])
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
                reducer.eval(parWithRand.body)(env, Blake2b512Random.merge(randoms), sequenceNumber)
              case ScalaBodyRef(ref) =>
                _dispatchTable.get(ref) match {
                  case Some(f) =>
                    f(
                      dataList.map(dl => ListParWithRandomAndPhlos(dl.pars, dl.randomState)),
                      sequenceNumber
                    )
                  case None => s.raiseError(new Exception(s"dispatch: no function for $ref"))
                }
              case Empty =>
                s.unit
            }
    } yield res

}

object RholangAndScalaDispatcher {

  def create[M[_], F[_]](
      tuplespace: RhoISpace[M],
      dispatchTable: => Map[Long, (Seq[ListParWithRandomAndPhlos], Int) => M[Unit]],
      urnMap: Map[String, Par]
  )(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]
  ): (Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation], ChargingReducer[M], Registry[M]) = {
    lazy val chargingRSpace = ChargingRSpace.pureRSpace(s, costAlg, tuplespace)
    lazy val tuplespaceAlg  = Tuplespace.rspaceTuplespace(chargingRSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation] =
      new RholangAndScalaDispatcher(chargingReducer, dispatchTable)
    implicit lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)
    implicit lazy val costAlg      = CostAccounting.unsafe[M](CostAccount(0))
    lazy val chargingReducer       = new ChargingReducer[M]
    lazy val registry: Registry[M] = new RegistryImpl(chargingRSpace, dispatcher)
    (dispatcher, chargingReducer, registry)
  }
}
