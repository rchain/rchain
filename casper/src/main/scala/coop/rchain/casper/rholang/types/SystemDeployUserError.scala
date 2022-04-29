package coop.rchain.casper.rholang.types

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.PrettyPrinter
import coop.rchain.rholang.interpreter.errors.InterpreterError

final case class SystemDeployUserError(errorMessage: String)

/**
  * Fatal error - node should exit on these errors.
  */
sealed abstract class SystemDeployPlatformFailure(errorMessage: String)
    extends RuntimeException(s"Platform failure: $errorMessage")

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
