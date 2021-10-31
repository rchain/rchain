package coop.rchain.casper.v2.processing
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.casper.v2.processing.MessageReceiver.ReceiveResult
import coop.rchain.casper.v2.processing.MessageValidator.ValidationResult
import coop.rchain.shared.syntax._
import fs2.Stream

/**
  * Definition for Casper messages processing.
  * @tparam M Message type
  * @tparam S Processing state type
  */
final case class MessageProcessor[F[_], M, S](
    receiver: MessageReceiver[F, M, S],
    retriever: DependenciesRetriever[F, M],
    validator: MessageValidator[F, M, S]
) {

  /**
    * Processing pipe for messages received from the network layer.
    * @param attemptPropose Function to make propose attempt.
    * @return Stream of Casper states after each message processed. The output is not that valuable because
    *         message processing should happen always on top of the latest state, but still might be useful.
    */
  def stream(attemptPropose: F[Unit])(implicit c: Concurrent[F]): Stream[F, S] = {
    val pullIncoming = receiver.input
    // Check and store incoming messages concurrently
      .parEvalMapProcBounded { m => // Todo not ack retrieved if sig is invalid
        retriever.ackRetrieved(m) >> receiver.checkIgnore(m).semiflatTap(receiver.store).value
      }
      // Invoke effect sequentially
      .evalMap {
        case Left(reason) => receiver.diagRejected(reason)
        case Right(m) =>
          receiver.receivedEffect(m).flatMap {
            case ReceiveResult(_, _, r) if r.nonEmpty => retriever.retrieve(r)
            case ReceiveResult(_, p, _)               => validator.appendToInput(m).whenA(p.isEmpty)
          }
      }

    val doValidate = validator.input
    // Validate all messages concurrently
      .parEvalMapProcBounded(m => validator.validate(m))
      // Start validation of unlocked children
      .evalMap {
        case ValidationResult(newSt, childrenUnlocked) =>
          childrenUnlocked.toList.traverse(validator.appendToInput).as(newSt)

      }
      // Attempt to propose after each validation
      .evalTap(_ => attemptPropose)

    doValidate concurrently pullIncoming
  }
}

object MessageProcessor {
  type MessageProcessingStream[F[_], S] = Stream[F, S]
}
