package coop.rchain.shared
import java.time.{LocalDate, LocalDateTime}

import scala.language.higherKinds

trait DateTime[F[_]] {
  def dateTime: F[LocalDateTime]
  def date: F[LocalDate]
}

object DateTime {
  def apply[F[_]](implicit dateTime: DateTime[F]): DateTime[F] = dateTime
}
