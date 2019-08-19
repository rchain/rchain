package coop.rchain.rspace.bench

import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.ChargingReducer
import java.io.{FileNotFoundException, InputStreamReader}
import monix.eval.Task

package object wide {

  def processErrors(errors: Option[Throwable]): Option[Throwable] = {

    if (errors.nonEmpty) {
      errors.foreach(_.printStackTrace())
      throw new RuntimeException(
        errors
          .map(_.toString())
          .mkString("Errors received during evaluation:\n", "\n", "\n")
      )
    }
    errors
  }

  def createTest(t: Option[Par])(
      implicit errorProcessor: () => Option[Throwable],
      reducer: ChargingReducer[Task],
      rand: Blake2b512Random
  ): Task[Option[Throwable]] = {
    val par = t.getOrElse(throw new Error("Failed to prepare executable rholang term"))
    reducer
      .inj(par)
      .map(_ => errorProcessor())
  }

  def resourceFileReader(path: String): InputStreamReader =
    new InputStreamReader(
      Option(getClass.getResourceAsStream(path))
        .getOrElse(throw new FileNotFoundException(path))
    )
}
