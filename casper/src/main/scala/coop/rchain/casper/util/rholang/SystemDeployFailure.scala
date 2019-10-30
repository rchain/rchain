package coop.rchain.casper.util.rholang

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rholang.interpreter.PrettyPrinter

sealed trait SystemDeployFailure

sealed abstract class SystemDeployUserError extends SystemDeployFailure

object SystemDeployUserError {
  final case class SystemDeployError(errorMsg: String) extends SystemDeployUserError
  def apply(errorMsg: String) = SystemDeployError(errorMsg)
}

sealed abstract class SystemDeployPlatformFailure(errorMessage: String)
    extends RuntimeException(s"Platform failure: $errorMessage")
    with SystemDeployFailure

object SystemDeployPlatformFailure {
  private def showSeqPar(pars: Seq[Par]) = pars match {
    case Seq()       => "Nil"
    case Seq(single) => PrettyPrinter().buildChannelString(single)
    case _           => pars.mkString("(", ",\n ", ")")
  }

  final case class UnexpectedResult(result: Seq[Par])
      extends SystemDeployPlatformFailure(s"Unable to proceed with ${showSeqPar(result)}")
  final case class UnexpectedSystemErrors(errors: Vector[InterpreterError])
      extends SystemDeployPlatformFailure(
        s"Caught errors in Rholang interpreter ${errors.mkString("[", ", ", "]")}"
      )
  final case class GasRefundFailure(errorMsg: String)
      extends SystemDeployPlatformFailure(s"Unable to refund remaining gas ($errorMsg)")
  case object ConsumeFailed
      extends SystemDeployPlatformFailure("Unable to consume results of system deploy")
}
