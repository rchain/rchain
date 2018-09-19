package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.implicits._
import cats.mtl.FunctorTell
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime.RhoISpace
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.storage.TuplespaceAlg
import coop.rchain.rspace.pure.PureRSpace
import monix.eval.Task

trait Dispatch[M[_], A, K] {
  def dispatch(continuation: K, dataList: Seq[A]): M[Unit]
}

object Dispatch {

  // TODO: Make this function total
  def buildEnv(dataList: Seq[ListChannelWithRandom]): Env[Par] =
    Env.makeEnv(
      dataList
        .flatMap(_.channels)
        .map({
          case Channel(Quote(p)) => p
          case Channel(_)        => Par() // Should never happen
        }): _*)
}

class RholangOnlyDispatcher[M[_]] private (reducer: => Reduce[M])(implicit s: Sync[M])
    extends Dispatch[M, ListChannelWithRandom, TaggedContinuation] {

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListChannelWithRandom]): M[Unit] =
    for {
      costAccountingAlg <- CostAccountingAlg.of(
                            dataList
                              .flatMap(_.cost)
                              .map(CostAccount.fromProto(_))
                              .toList
                              .combineAll)
      res <- continuation.taggedCont match {
              case ParBody(parWithRand) =>
                val env     = Dispatch.buildEnv(dataList)
                val randoms = parWithRand.randomState +: dataList.toVector.map(_.randomState)
                reducer.eval(parWithRand.body)(env,
                                               Blake2b512Random.merge(randoms),
                                               costAccountingAlg)
              case ScalaBodyRef(_) =>
                s.unit
              case Empty =>
                s.unit
            }
    } yield res
}

object RholangOnlyDispatcher {

  def create[M[_], F[_]](tuplespace: RhoISpace, urnMap: Map[String, Par] = Map.empty)(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable])
    : (Dispatch[M, ListChannelWithRandom, TaggedContinuation], Reduce[M]) = {
    val pureSpace          = PureRSpace[M].of(tuplespace)
    lazy val tuplespaceAlg = TuplespaceAlg.rspaceTuplespace(pureSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, ListChannelWithRandom, TaggedContinuation] =
      new RholangOnlyDispatcher(reducer)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)
    (dispatcher, reducer)
  }
}

class RholangAndScalaDispatcher[M[_]] private (
    reducer: => Reduce[M],
    _dispatchTable: => Map[Long, Function1[Seq[ListChannelWithRandom], M[Unit]]])(
    implicit s: Sync[M])
    extends Dispatch[M, ListChannelWithRandom, TaggedContinuation] {

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListChannelWithRandom]): M[Unit] =
    for {
      costAccountingAlg <- CostAccountingAlg.of(
                            dataList
                              .flatMap(_.cost)
                              .map(CostAccount.fromProto(_))
                              .toList
                              .combineAll)
      res <- continuation.taggedCont match {
              case ParBody(parWithRand) =>
                val env     = Dispatch.buildEnv(dataList)
                val randoms = parWithRand.randomState +: dataList.toVector.map(_.randomState)
                reducer.eval(parWithRand.body)(env,
                                               Blake2b512Random.merge(randoms),
                                               costAccountingAlg)
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

  def create(tuplespace: RhoISpace,
             dispatchTable: => Map[Long, Function1[Seq[ListChannelWithRandom], Task[Unit]]],
             urnMap: Map[String, Par])(implicit ft: FunctorTell[Task, Throwable])
    : (Dispatch[Task, ListChannelWithRandom, TaggedContinuation], Reduce[Task], Registry) = {
    val pureSpace          = PureRSpace[Task].of(tuplespace)
    lazy val tuplespaceAlg = TuplespaceAlg.rspaceTuplespace(pureSpace, dispatcher)
    lazy val dispatcher: Dispatch[Task, ListChannelWithRandom, TaggedContinuation] =
      new RholangAndScalaDispatcher(reducer, dispatchTable)
    lazy val reducer: Reduce[Task] =
      new Reduce.DebruijnInterpreter[Task, Task.Par](tuplespaceAlg, urnMap)
    lazy val registry: Registry = new Registry(pureSpace, dispatcher)
    (dispatcher, reducer, registry)
  }
}
