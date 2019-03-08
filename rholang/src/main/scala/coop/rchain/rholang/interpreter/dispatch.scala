package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.mtl.FunctorTell
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.{RhoISpace, RhoPureSpace}
import coop.rchain.rholang.interpreter.accounting._
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
    _dispatchTable: => Map[Long, (Seq[ListParWithRandomAndPhlos], Int) => M[Unit]]
)(implicit s: Sync[M], reducer: ChargingReducer[M])
    extends Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation] {

  def dispatch(
      continuation: TaggedContinuation,
      dataList: Seq[ListParWithRandomAndPhlos],
      sequenceNumber: Int
  ): M[Unit] =
    continuation.taggedCont match {
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
}

object RholangAndScalaDispatcher {

  def createWithEmptyCost[M[_], F[_]](
      tuplespace: RhoISpace[M],
      dispatchTable: => Map[Long, (Seq[ListParWithRandomAndPhlos], Int) => M[Unit]],
      urnMap: Map[String, Par]
  )(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]
  ): (Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation], ChargingReducer[M], Registry[M]) = {
    implicit val costAlg: CostAccounting[M] = CostAccounting.unsafe[M](Cost(0))
    implicit val cost: _cost[M]             = loggingCost(costAlg, noOpCostLog)
    create(tuplespace, dispatchTable, urnMap)

  }

  def create[M[_], F[_]](
      tuplespace: RhoISpace[M],
      dispatchTable: => Map[Long, (Seq[ListParWithRandomAndPhlos], Int) => M[Unit]],
      urnMap: Map[String, Par]
  )(
      implicit
      cost: _cost[M],
      costAccounting: CostAccounting[M],
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]
  ): (Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation], ChargingReducer[M], Registry[M]) = {

    implicit lazy val dispatcher: Dispatch[M, ListParWithRandomAndPhlos, TaggedContinuation] =
      new RholangAndScalaDispatcher(dispatchTable)

    implicit lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)

    lazy val tuplespaceAlg = Tuplespace.rspaceTuplespace(chargingRSpace, dispatcher)

    lazy val chargingRSpace: RhoPureSpace[M] =
      ChargingRSpace.pureRSpace(s, costAccounting, tuplespace)

    val chargingReducer: ChargingReducer[M] = ChargingReducer[M]

    val registry: Registry[M] = new RegistryImpl(chargingRSpace, dispatcher)
    (dispatcher, chargingReducer, registry)
  }
}
