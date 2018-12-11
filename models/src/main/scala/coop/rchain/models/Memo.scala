package coop.rchain.models
import com.typesafe.scalalogging.Logger
import monix.eval.Coeval
import monix.eval.Coeval.Eager

class Memo[A](f: => Coeval[A]) {

  private[this] val logger = Logger("Memo")

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
                logger.warn(s"Non-Eager result when the thunk is null: $result")
                result
            }
          }
        }
    }
  }
}
