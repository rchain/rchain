package coop.rchain.node

import monix.eval.Task
import org.http4s.Status
import org.http4s.dsl.impl.EntityResponseGenerator

object Http4sDsl {
  def OkF[F[_]]: EntityResponseGenerator[F] =
    new EntityResponseGenerator[F] {
      def status: Status = Status(200, "OK")
    }

  val Ok: EntityResponseGenerator[Task] = OkF[Task]
}
