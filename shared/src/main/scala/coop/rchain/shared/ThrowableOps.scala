package coop.rchain.shared

import scala.annotation.tailrec

object ThrowableOps {
  implicit class RichThrowable(th: Throwable) {
    def containsMessageWith(str: String): Boolean =
      (Option(th.getCause), Option(th.getMessage)) match {
        case (_, Some(msg)) if msg.contains(str) => true
        case (Some(cause), _)                    => cause.containsMessageWith(str)
        case _                                   => false
      }

    def fold[B](z: B)(f: (B, Throwable) => B): B = {
      @tailrec
      def loop(t0: Throwable, acc: B): B =
        Option(t0) match {
          case None     => acc
          case Some(t1) => loop(t1.getCause, f(acc, t1))
        }

      loop(th, z)
    }

    def toMessageList(prefix: String = "Caused by: "): Seq[String] =
      fold(Seq.empty[String]) { (ms, t) =>
        Option(t.getMessage)
          .filter(_.trim.length > 0)
          .fold(ms)(msg => s"${if (ms.isEmpty) "" else prefix}$msg" +: ms)
      }.reverse

  }
}
