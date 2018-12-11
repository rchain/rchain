package coop.rchain.catscontrib
import cats.Id

object TestOutlaws {

  /** TEMP REMOVE once comm no longer imperative*/
  implicit val idCapture: Capture[Id] = new Capture[Id] {
    def capture[A](a: => A): Id[A] = a
  }
  /** TEMP REMOVE END */
}
