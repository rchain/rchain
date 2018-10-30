package coop.rchain.node

import monix.eval.Task
import org.http4s.Status
import org.http4s.dsl.impl.EntityResponseGenerator

object Ok extends EntityResponseGenerator[Task] {
  override def status: Status = Status(200, "OK")
}
