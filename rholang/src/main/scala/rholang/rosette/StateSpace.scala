package AbstractInterpreter

import ADT.Proc
import AbstractInterpreter.StateSpace.{Env, Var}
import monix.eval.{MVar, Task}

import scala.collection.immutable.HashMap

package object StateSpace {

  type Var = String
  type Env[A] = HashMap[Var,A]
  type Store[A] = HashMap[A,Val[A]]

}

/*
 * Note: if IOAddr were generic, we'd be able to
 * store arbitrary values, and type-check addresses against the
 * types of their associated values.
 */

case class IOAddr(var value: Option[Quote]){
  def lookup: Quote = value match {
    case Some(data) => data
    case None => sys.error("Null pointer exception")
  }
}

case class Val[A](env: Env[A], proc: Proc[Var])

// Representing closures
object Val {
  implicit class valOps(value: Val[IOAddr]){
    def quote: Quote = Quote(value)(MVar.empty[Val[IOAddr]])
  }
}

// Channel generation
case class Quote(unquote: Val[IOAddr])(private val mVar: MVar[Val[IOAddr]]) extends MVar[Val[IOAddr]] {
  def put(a: Val[IOAddr]): Task[Unit] = mVar.put(a)
  def take: Task[Val[IOAddr]] = mVar.take
  def read: Task[Val[IOAddr]] = mVar.read
}


