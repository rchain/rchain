package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models.TaggedContinuation.TaggedCont.{Empty, ParBody, ScalaBodyRef}
import coop.rchain.models.{Channel, Par, TaggedContinuation}
import coop.rchain.rspace.IStore
import monix.eval.Task

trait Dispatch[M[_], A, K] {

  val reducer: Reduce[M]

  def dispatch(continuation: K, dataList: List[A]): M[Unit]
}

class RholangOnlyDispatcher private (_reducer: => Reduce[Task])
    extends Dispatch[Task, Seq[Channel], TaggedContinuation] {

  val reducer: Reduce[Task] = _reducer

  // TODO: Make this function total
  private def buildEnv(dataList: List[Seq[Channel]]): Env[Par] =
    Env.makeEnv(dataList.flatten.map({ case Channel(Quote(p)) => p }): _*)

  def dispatch(continuation: TaggedContinuation, dataList: List[Seq[Channel]]): Task[Unit] =
    continuation.taggedCont match {
      case ParBody(par) =>
        val env = buildEnv(dataList)
        reducer.eval(par)(env)
      case ScalaBodyRef(_) =>
        Task.unit
      case Empty =>
        Task.unit
    }
}

object RholangOnlyDispatcher {

  def create(tuplespace: IStore[Channel, Seq[Channel], Seq[Channel], TaggedContinuation])
    : Dispatch[Task, Seq[Channel], TaggedContinuation] = {
    lazy val dispatcher: Dispatch[Task, Seq[Channel], TaggedContinuation] =
      new RholangOnlyDispatcher(reducer)
    lazy val reducer: Reduce[Task] =
      new Reduce.DebruijnInterpreter(tuplespace, dispatcher)
    dispatcher
  }
}
