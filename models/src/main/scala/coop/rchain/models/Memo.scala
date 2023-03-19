package coop.rchain.models
import com.typesafe.scalalogging.Logger
import cats.{Eval, Now}

@SuppressWarnings(Array("org.wartremover.warts.Var"))
class Memo[A](f: => Eval[A]) {

  private[this] var thunk           = f
  private[this] var result: Eval[A] = _

  def get: Eval[A] = synchronized {
    result match {
      case e: Now[A] => e
      case _ =>
        Eval.defer {
          synchronized {
            result match {
              case e: Now[A] => e
              case _ if thunk != null =>
                thunk.map { r =>
                  synchronized {
                    thunk = null //allow GC-ing the thunk
                    result = Eval.now(r)
                    r
                  }
                }
              case _ =>
                Memo.logger.warn(s"Non-Eager result when the thunk is null: $result")
                result
            }
          }
        }
    }
  }
}

object Memo {
  private[Memo] val logger = Logger("Memo")
}
