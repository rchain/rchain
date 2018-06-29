package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.Sync
import cats.mtl.FunctorTell
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models.{BindPattern, Channel, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.accounting.CostAccountingAlg
import coop.rchain.rholang.interpreter.storage.TuplespaceAlg
import coop.rchain.rspace.ISpace
import coop.rchain.rspace.pure.PureRSpace

trait Dispatch[M[_], A, K] {

  val reducer: Reduce[M]

  def dispatch(continuation: K, dataList: Seq[A]): M[Unit]
}

object Dispatch {

  // TODO: Make this function total
  def buildEnv(dataList: Seq[Seq[Channel]]): Env[Par] =
    Env.makeEnv(dataList.flatten.map({
      case Channel(Quote(p)) => p
      case Channel(_)        => Par() // Should never happen
    }): _*)
}

class RholangOnlyDispatcher[M[_]] private (_reducer: => Reduce[M])(implicit s: Sync[M])
    extends Dispatch[M, Seq[Channel], TaggedContinuation] {

  val reducer: Reduce[M] = _reducer

  def dispatch(continuation: TaggedContinuation, dataList: Seq[Seq[Channel]]): M[Unit] =
    continuation.taggedCont match {
      case ParBody(par) =>
        val env = Dispatch.buildEnv(dataList)
        reducer.eval(par)(env)
      case ScalaBodyRef(_) =>
        s.unit
      case Empty =>
        s.unit
    }
}

object RholangOnlyDispatcher {

  def create[M[_], F[_]](
      tuplespace: ISpace[Channel, BindPattern, Seq[Channel], Seq[Channel], TaggedContinuation],
      costAccountingAlg: CostAccountingAlg[M])(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]): Dispatch[M, Seq[Channel], TaggedContinuation] = {
    val pureSpace
      : PureRSpace[M, Channel, BindPattern, Seq[Channel], Seq[Channel], TaggedContinuation] =
      new PureRSpace(tuplespace)
    lazy val tuplespaceAlg = TuplespaceAlg.rspaceTuplespace(pureSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, Seq[Channel], TaggedContinuation] =
      new RholangOnlyDispatcher(reducer)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, costAccountingAlg)
    dispatcher
  }
}

class RholangAndScalaDispatcher[M[_]] private (
    _reducer: => Reduce[M],
    _dispatchTable: => Map[Long, Function1[Seq[Seq[Channel]], M[Unit]]])(implicit s: Sync[M])
    extends Dispatch[M, Seq[Channel], TaggedContinuation] {

  val reducer: Reduce[M] = _reducer

  def dispatch(continuation: TaggedContinuation, dataList: Seq[Seq[Channel]]): M[Unit] =
    continuation.taggedCont match {
      case ParBody(par) =>
        val env = Dispatch.buildEnv(dataList)
        reducer.eval(par)(env)
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

  def create[M[_], F[_]](
      tuplespace: ISpace[Channel, BindPattern, Seq[Channel], Seq[Channel], TaggedContinuation],
      dispatchTable: => Map[Long, Function1[Seq[Seq[Channel]], M[Unit]]],
      costAccountingAlg: CostAccountingAlg[M])(
      implicit
      parallel: Parallel[M, F],
      s: Sync[M],
      ft: FunctorTell[M, Throwable]): Dispatch[M, Seq[Channel], TaggedContinuation] = {
    val pureSpace
      : PureRSpace[M, Channel, BindPattern, Seq[Channel], Seq[Channel], TaggedContinuation] =
      new PureRSpace(tuplespace)
    lazy val tuplespaceAlg = TuplespaceAlg.rspaceTuplespace(pureSpace, dispatcher)
    lazy val dispatcher: Dispatch[M, Seq[Channel], TaggedContinuation] =
      new RholangAndScalaDispatcher(reducer, dispatchTable)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](tuplespaceAlg, costAccountingAlg)
    dispatcher
  }
}
