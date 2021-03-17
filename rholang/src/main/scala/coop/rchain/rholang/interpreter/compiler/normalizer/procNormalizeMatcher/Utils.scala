package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance.{ConnNotBody, ConnOrBody}
import coop.rchain.rholang.interpreter.compiler.{NameVisitOutputs, ProcVisitInputs}
import coop.rchain.rholang.interpreter.errors.{InterpreterError, PatternReceiveError}

object Utils {
  def failOnInvalidConnective(
      input: ProcVisitInputs,
      depth: Int,
      nameRes: NameVisitOutputs
  ): Either[InterpreterError, NameVisitOutputs] =
    if (input.env.depth == 0) {
      Either
        .fromOption(
          nameRes.knownFree.connectives
            .collectFirst {
              case (_: ConnOrBody, sourcePosition) =>
                PatternReceiveError(s"\\/ (disjunction) at $sourcePosition")
              case (_: ConnNotBody, sourcePosition) =>
                PatternReceiveError(s"~ (negation) at $sourcePosition")
            },
          nameRes
        )
        .swap
    } else Right(nameRes)
}
