package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.mtl.FunctorTell
import coop.rchain.catscontrib.Capture
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models.{BindPattern, Channel, ListChannelWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.errors.{InterpreterError, InterpreterErrorsM}
import coop.rchain.rspace.ISpace
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

class RholangOnlyDispatcher[M[_]] private (_reducer: => Reduce[M])(implicit captureM: Capture[M])
    extends Dispatch[M, ListChannelWithRandom, TaggedContinuation] {

  val reducer: Reduce[M] = _reducer

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListChannelWithRandom]): M[Unit] =
    continuation.taggedCont match {
      case ParBody(parWithRand) =>
        val env     = Dispatch.buildEnv(dataList)
        val randoms = parWithRand.randomState +: dataList.toVector.map(_.randomState)
        reducer.eval(parWithRand.body)(env, Blake2b512Random.merge(randoms))
      case ScalaBodyRef(_) =>
        captureM.apply(())
      case Empty =>
        captureM.apply(())
    }
}

object RholangOnlyDispatcher {

  def create[M[_], F[_]](tuplespace: ISpace[Channel,
                                            BindPattern,
                                            ListChannelWithRandom,
                                            ListChannelWithRandom,
                                            TaggedContinuation])(
      implicit
      intepreterErrorsM: InterpreterErrorsM[M],
      captureM: Capture[M],
      parallel: Parallel[M, F],
      ft: FunctorTell[M, InterpreterError])
    : Dispatch[M, ListChannelWithRandom, TaggedContinuation] = {
    val pureSpace: PureRSpace[M,
                              Channel,
                              BindPattern,
                              ListChannelWithRandom,
                              ListChannelWithRandom,
                              TaggedContinuation] =
      new PureRSpace(tuplespace)
    lazy val dispatcher: Dispatch[M, ListChannelWithRandom, TaggedContinuation] =
      new RholangOnlyDispatcher(reducer)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](pureSpace, dispatcher)
    dispatcher
  }
}

class RholangAndScalaDispatcher[M[_]] private (
    _reducer: => Reduce[M],
    _dispatchTable: => Map[Long, Function1[Seq[ListChannelWithRandom], M[Unit]]])(
    implicit captureM: Capture[M])
    extends Dispatch[M, ListChannelWithRandom, TaggedContinuation] {

  val reducer: Reduce[M] = _reducer

  def dispatch(continuation: TaggedContinuation, dataList: Seq[ListChannelWithRandom]): M[Unit] =
    continuation.taggedCont match {
      case ParBody(parWithRand) =>
        val env     = Dispatch.buildEnv(dataList)
        val randoms = parWithRand.randomState +: dataList.toVector.map(_.randomState)
        reducer.eval(parWithRand.body)(env, Blake2b512Random.merge(randoms))
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

  def create[M[_], F[_]](
      tuplespace: ISpace[Channel,
                         BindPattern,
                         ListChannelWithRandom,
                         ListChannelWithRandom,
                         TaggedContinuation],
      dispatchTable: => Map[Long, Function1[Seq[ListChannelWithRandom], M[Unit]]])(
      implicit
      intepreterErrorsM: InterpreterErrorsM[M],
      captureM: Capture[M],
      parallel: Parallel[M, F],
      ft: FunctorTell[M, InterpreterError])
    : Dispatch[M, ListChannelWithRandom, TaggedContinuation] = {
    val pureSpace: PureRSpace[M,
                              Channel,
                              BindPattern,
                              ListChannelWithRandom,
                              ListChannelWithRandom,
                              TaggedContinuation] =
      new PureRSpace(tuplespace)
    lazy val dispatcher: Dispatch[M, ListChannelWithRandom, TaggedContinuation] =
      new RholangAndScalaDispatcher(reducer, dispatchTable)
    lazy val reducer: Reduce[M] =
      new Reduce.DebruijnInterpreter[M, F](pureSpace, dispatcher)
    dispatcher
  }
}
