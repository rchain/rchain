package coop.rchain.v2.validation
import cats.Applicative
import cats.syntax.all._
import coop.rchain.shared.Time

/**
 * Offences are to be detected on a properly signed message, which means some private key is backing the message.
 * As offence is always issued against some sender, messages unsigned / having incorrect signatures are filtered before.
 */
trait Offence

case object Offence {
  val DRIFT = 15000 // 15 seconds

  /**
   * If sender creates and signs messages that are not well formed, there are two options: just ignore, or act in
   * some way. Just ignoring introduces DoS attack vector when lots of blocks have to be verified, because the whole
   * message verification takes more resources then just verifying the signature.
   * Therefore ill formed message is a slashing offence.
   */
  def malformedMessage[M](m: M, checkFormat: M => Boolean): Boolean = checkFormat(m)

  /**
   * Message should have a timestamp. Ths gives a (quite relaxed) notion of a time.
   * Finalization fringe messages can give a answer on whether certain time point has passed.
   */
  def invalidTimestamp[F[_]: Applicative: Time, M](m: M, parents: Set[M])(
      timestamp: M => Long
  ): F[Boolean] =
    Time[F].currentMillis.map { now =>
      val allowedRange = (parents.map(timestamp).max, now + DRIFT)
      val ts           = timestamp(m)
      (allowedRange._1 < ts) && (ts < allowedRange._2)
    }

  /**
   * Message should have justification for each senders in the bonds map (both active and inactive).
   */
  def incompleteJustificationSet[M, S](m: M, bondedSenders: Set[S])(
      justifications: M => Set[M],
      sender: M => S
  ): Boolean = justifications(m).map(sender) == bondedSenders

  /**
   * Message should have sequence number equal to sequence number of self justification + 1.
   */
  def invalidSequenceNumber[M](m: M, selfJustification: M)(seqNum: M => Long) =
    seqNum(m) != seqNum(selfJustification) + 1

  /**
   * Casper message should not have justifications past to justification of previous message from the same sender.
   */
  def justificationRegression[M, S](
      m: M
  )(justifications: M => Set[M], sender: M => S, seqNum: M => Long): Boolean = {
    val js       = justifications(m)
    val selfJOpt = js.find(sender(_) == sender(m))
    assert(selfJOpt.isDefined, "Message does not have self justification.")
    val selfJ    = selfJOpt.get
    val selfJjs  = justifications(selfJ)
    js.toIterator.map { j =>
      selfJjs.exists { selfJj =>
        // Previous justification from the same sender has seq num higher then the current one.
        sender(selfJj) == sender(j) && seqNum(selfJj) > seqNum(j)
      }
    }.nonEmpty
  }

  /**
   * Offence that leads to slashing of the offender.
   */
  trait SlashableOffence extends Offence
}

//case object MalformedMessage extends SlashableOffence

//case object IncompleteJustification extends SlashableOffence

//case object JustificationRegression extends SlashableOffence

/**
 * Sequence number of a block have to be equal sequence number of self justification + 1.
 */
case object InvalidSequenceNumber extends SlashableOffence

/**
 * Sender created two blocks that are conflicting with each other.
 */
case object Equivocation extends SlashableOffence

object Offence {

  def invalidFormat: Offence = InvalidFormat

  def invalidSignature: Offence = InvalidSignature

  def invalidSender: Offence = InvalidSender

  def invalidVersion: Offence = InvalidVersion

  def invalidTimestamp: Offence = InvalidTimestamp

  def deployNotSigned: Offence = DeployNotSigned

  def invalidBlockNumber: Offence = InvalidBlockNumber

  def invalidRepeatDeploy: Offence = InvalidRepeatDeploy

  def invalidParents: Offence = InvalidParents

  def invalidFollows: Offence = InvalidFollows

  def invalidSequenceNumber: Offence = InvalidSequenceNumber

  def invalidShardId: Offence = InvalidShardId

  def justificationRegression: Offence = JustificationRegression

  def neglectedInvalidBlock: Offence = NeglectedInvalidBlock

  def neglectedEquivocation: Offence = NeglectedEquivocation

  def invalidTransaction: Offence = InvalidTransaction

  def invalidMessageScope: Offence = InvalidMessageScope

//  case object InvalidFormat    extends MalformedBlock
//  case object InvalidSender    extends MalformedBlock
//  case object InvalidVersion   extends MalformedBlock
//  case object DeployNotSigned  extends MalformedBlock
//  case object InvalidBlockHash extends MalformedBlock

  def invalidBondsCache: Offence = InvalidBondsCache

  def invalidBlockHash: Offence = InvalidBlockHash

  def containsExpiredDeploy: Offence = ContainsExpiredDeploy

  def containsFutureDeploy: Offence = ContainsFutureDeploy

  def invalidRejectedDeploy: Offence = InvalidRejectedDeploy

  def notOfInterest: Offence = NotOfInterest

  final case class Slashing[M](message: M, offence: SlashableOffence)

  case object DummyOffence extends SlashableOffence // TODO remove and store concrete Offence

  case object AdmissibleEquivocation extends SlashableOffence

  case object IgnorableEquivocation extends SlashableOffence

  case object InvalidBlockNumber extends SlashableOffence

  case object InvalidRepeatDeploy extends SlashableOffence

  case object InvalidParents extends SlashableOffence

  case object InvalidFollows extends SlashableOffence

  case object InvalidShardId extends SlashableOffence

  case object JustificationRegression extends SlashableOffence

  case object NeglectedInvalidBlock extends SlashableOffence

  case object NeglectedEquivocation extends SlashableOffence

  case object InvalidTransaction extends SlashableOffence

  case object InvalidMessageScope extends SlashableOffence

  case object InvalidBondsCache extends SlashableOffence

  case object ContainsExpiredDeploy extends SlashableOffence

  case object ContainsFutureDeploy extends SlashableOffence

  case object InvalidRejectedDeploy extends SlashableOffence

  case object NotOfInterest extends SlashableOffence
}
