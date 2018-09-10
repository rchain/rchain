package coop.rchain.rholang.interpreter
import java.io.StringReader

import coop.rchain.rholang.interpreter.errors.UnrecognizedInterpreterError
import org.scalatest.{FlatSpec, Matchers}

class ErrorsSpec extends FlatSpec with Matchers {

  "Printing stack trace from rholang interpreter" should "not go into infinite loop causing SOE" in {
    val invalidRholang = "@\"network!(10)"
    try {
      Interpreter.buildNormalizedTerm(new StringReader(invalidRholang)).value
    } catch {
      case ex: UnrecognizedInterpreterError =>
        try {
          ex.printStackTrace(System.err)
          assert(true)
        } catch {
          case _: StackOverflowError =>
            // fail the test when SOE is thrown when printing the error stacktrace
            assert(false)
        }
      case _ =>
        // if error different than expected UnrecognizedInterpreterError was thrown also fail
        assert(false)
    }
  }

}
