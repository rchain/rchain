package coop.rchain.models
import com.typesafe.scalalogging.Logger
import monix.eval.Coeval
import monix.eval.Coeval.Eager

@SuppressWarnings(Array("org.wartremover.warts.Var"))
class Memo[A](f: => Coeval[A]) {

  private[this] var thunk             = f
  private[this] var result: Coeval[A] = _

  def get: Coeval[A] = synchronized {
    result match {
      case e: Eager[A] => e
      case _ =>
        Coeval.defer {
          synchronized {
            result match {
              case e: Eager[A] => e
              case _ if thunk != null =>
                thunk.map { r =>
                  synchronized {
                    thunk = null //allow GC-ing the thunk
                    result = Coeval.now(r)
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
