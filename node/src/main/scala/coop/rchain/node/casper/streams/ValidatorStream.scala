package coop.rchain.node.casper.streams

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.shared.syntax._
import coop.rchain.v2.casperclient.CasperValidator
import coop.rchain.v2.casperclient.CasperValidator.ValidateEffects
import coop.rchain.v2.validation.Offence.Slashing
import fs2.Stream

object ValidatorStream {

  /**
   * Validation stream.
   *
   * @param input         Input stream of messages. All dependencies should be validated before message appears
   *                      in this stream.
   * @param appendToInput Validation of a message can trigger validation of a child, so being able to append to
   *                      input is required.
   * @param validator     Instance of Casper validator to pass message to.
   */
  def apply[F[_]: Concurrent, M, S](
      input: Stream[F, ValidationTarget[M]],
      appendToInput: ValidationTarget[M] => F[Unit],
      validator: CasperValidator[F, M, S],
      validationEffect: (M, Option[Slashing[M]]) => F[Unit],
      proposeF: F[Unit]
  ): Stream[F, Option[Slashing[M]]] =
    input
      // Validate all messages concurrently
      .parEvalMapProcBounded(m => validator.validate(m.message, m.selfProposed).map((_, m)))
      // Invoke validation effect, attempt to propose and trigger validation of dependency free children
      .evalMap { case (ValidateEffects(_, slashing, childrenUnlocked), t) =>
        for {
          _ <- CasperValidator.invalidSelfMessage.whenA(t.selfProposed && slashing.nonEmpty)
          _ <- validationEffect(t.message, slashing)
          _ <- proposeF *> childrenUnlocked
                 .traverse(m => appendToInput(ValidationTarget(m, selfProposed = false)))
        } yield slashing
      }

  case class ValidationTarget[M](message: M, selfProposed: Boolean)
}
