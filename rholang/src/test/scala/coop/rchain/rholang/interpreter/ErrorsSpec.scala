package coop.rchain.rholang.interpreter
import java.io.{ByteArrayOutputStream, PrintStream}

import coop.rchain.rholang.interpreter.errors.{InterpreterError, UnrecognizedInterpreterError}
import org.scalacheck.ScalacheckShapeless._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ErrorsSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "Using Throwable methods on InterpreterError" should "not cause exceptions itself" in {
    forAll { e: InterpreterError =>
      checkThrowableMethodsAreSafe(e)
      checkThrowableMethodsAreSafe(UnrecognizedInterpreterError(e))
    }
  }

  private def checkThrowableMethodsAreSafe[T <: Throwable](e: T): Assertion =
    noException should be thrownBy {
      e.toString
      e.getMessage
      e.getCause
      val devNull = new PrintStream(new ByteArrayOutputStream())
      e.printStackTrace(devNull /* avoid garbage output in tests */ )
    }
}
