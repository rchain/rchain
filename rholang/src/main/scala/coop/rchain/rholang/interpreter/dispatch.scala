package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.mtl.FunctorTell
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccount, CostAccountingAlg}
import coop.rchain.rholang.interpreter.storage.TuplespaceAlg
import coop.rchain.rspace.ISpace
import cats.implicits._
import coop.rchain.rspace.pure.PureRSpace

trait Dispatch[M[_], A, K] {

  val reducer: Reduce[M]

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

class RholangOnlyDispatcher[M[_]] private (_reducer: => Reduce[M])(implicit s: Sync[M])
    extends Dispatch[M, ListChannelWithRandom, TaggedContinuation] {

  val reducer: Reduce[M] = _reducer

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListChannelWithRandom]): M[Unit] =
    for {
      costAccountingAlg <- CostAccountingAlg(
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

  def create[M[_], F[_]](tuplespace: ISpace[Channel,
                                            BindPattern,
                                            ListChannelWithRandom,
                                            ListChannelWithRandom,
                                            TaggedContinuation],
                         urnMap: Map[String, Par] = Map.empty)(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]): Dispatch[M, ListChannelWithRandom, TaggedContinuation] = {
    val pureSpace: PureRSpace[M,
                              Channel,
                              BindPattern,
                              ListChannelWithRandom,
                              ListChannelWithRandom,
                              TaggedContinuation] =
      new PureRSpace(tuplespace)
    lazy val tuplespaceAlg = TuplespaceAlg.rspaceTuplespace(pureSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, ListChannelWithRandom, TaggedContinuation] =
      new RholangOnlyDispatcher(reducer)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)
    dispatcher
  }
}

class RholangAndScalaDispatcher[M[_]] private (
    _reducer: => Reduce[M],
    _dispatchTable: => Map[Long, Function1[Seq[ListChannelWithRandom], M[Unit]]])(
    implicit s: Sync[M])
    extends Dispatch[M, ListChannelWithRandom, TaggedContinuation] {

  val reducer: Reduce[M] = _reducer

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListChannelWithRandom]): M[Unit] =
    for {
      costAccountingAlg <- CostAccountingAlg(
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

  def create[M[_], F[_]](
      tuplespace: ISpace[Channel,
                         BindPattern,
                         ListChannelWithRandom,
                         ListChannelWithRandom,
                         TaggedContinuation],
      dispatchTable: => Map[Long, Function1[Seq[ListChannelWithRandom], M[Unit]]],
      urnMap: Map[String, Par])(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]): Dispatch[M, ListChannelWithRandom, TaggedContinuation] = {
    val pureSpace: PureRSpace[M,
                              Channel,
                              BindPattern,
                              ListChannelWithRandom,
                              ListChannelWithRandom,
                              TaggedContinuation] =
      new PureRSpace(tuplespace)
    lazy val tuplespaceAlg = TuplespaceAlg.rspaceTuplespace(pureSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, ListChannelWithRandom, TaggedContinuation] =
      new RholangAndScalaDispatcher(reducer, dispatchTable)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, urnMap)
    dispatcher
  }
}
