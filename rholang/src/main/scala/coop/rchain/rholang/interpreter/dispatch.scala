package coop.rchain.rholang.interpreter

import cats.Parallel
import coop.rchain.catscontrib.Capture
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models.{BindPattern, Channel, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.errors.InterpreterErrorsM
import coop.rchain.rspace.ISpace
import coop.rchain.rspace.pure.PureRSpace

trait Dispatch[M[_], A, K] {

  val reducer: Reduce[M]

  def dispatch(continuation: K, dataList: Seq[A]): M[Unit]
}

object Dispatch {

  // TODO: Make this function total
  def buildEnv(dataList: Seq[Seq[Channel]]): Env[Par] =
    Env.makeEnv(dataList.flatten.map({ case Channel(Quote(p)) => p }): _*)
}

class RholangOnlyDispatcher[M[_]] private (_reducer: => Reduce[M])(implicit captureM: Capture[M])
    extends Dispatch[M, Seq[Channel], TaggedContinuation] {

  val reducer: Reduce[M] = _reducer

  def dispatch(continuation: TaggedContinuation, dataList: Seq[Seq[Channel]]): M[Unit] =
    continuation.taggedCont match {
      case ParBody(par) =>
        val env = Dispatch.buildEnv(dataList)
        reducer.eval(par)(env)
      case ScalaBodyRef(_) =>
        captureM.apply(())
      case Empty =>
        captureM.apply(())
    }
}

object RholangOnlyDispatcher {

  def create[M[_], F[_]](
      tuplespace: ISpace[Channel, BindPattern, Seq[Channel], TaggedContinuation])(
      implicit
      intepreterErrorsM: InterpreterErrorsM[M],
      captureM: Capture[M],
      parallel: Parallel[M, F]): Dispatch[M, Seq[Channel], TaggedContinuation] = {
    val pureSpace: PureRSpace[M, Channel, BindPattern, Seq[Channel], TaggedContinuation] =
      new PureRSpace(tuplespace)
    lazy val dispatcher: Dispatch[M, Seq[Channel], TaggedContinuation] =
      new RholangOnlyDispatcher(reducer)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](pureSpace, dispatcher)
    dispatcher
  }
}

class RholangAndScalaDispatcher[M[_]] private (
    _reducer: => Reduce[M],
    _dispatchTable: => Map[Long, Function1[Seq[Seq[Channel]], M[Unit]]])(
    implicit captureM: Capture[M])
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
          case None    => captureM.fail(new Exception(s"dispatch: no function for $ref"))
        }
      case Empty =>
        captureM.apply(())
    }
}

object RholangAndScalaDispatcher {

  def create[M[_], F[_]](tuplespace: ISpace[Channel, BindPattern, Seq[Channel], TaggedContinuation],
                         dispatchTable: => Map[Long, Function1[Seq[Seq[Channel]], M[Unit]]])(
      implicit
      intepreterErrorsM: InterpreterErrorsM[M],
      captureM: Capture[M],
      parallel: Parallel[M, F]): Dispatch[M, Seq[Channel], TaggedContinuation] = {
    val pureSpace: PureRSpace[M, Channel, BindPattern, Seq[Channel], TaggedContinuation] =
      new PureRSpace(tuplespace)
    lazy val dispatcher: Dispatch[M, Seq[Channel], TaggedContinuation] =
      new RholangAndScalaDispatcher(reducer, dispatchTable)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](pureSpace, dispatcher)
    dispatcher
  }
}
