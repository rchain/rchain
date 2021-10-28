package coop.rchain.v2.casper

import coop.rchain.v2.casper.data.FinalizationFringe
import coop.rchain.v2.casper.stcasper.StateMessage
import coop.rchain.v2.casper.syntax.all._

/**
 * Casper rules defining validity of a message.
 */
object Validation {

  /**
   * Message should be well formed.
   *
   * If sender creates and signs message that contains junk data, there are two options: just ignore, or act.
   * Just ignoring introduces DoS attack vector when lots of blocks have to be verified, because the whole
   * message verification takes more resources then just verifying the signature.
   * Therefore ill formed message is an offence.
   */
  def invalidMalformedMessage[M](
      m: M,
      checkFormat: M => MalformedMessageReason
  ): MalformedMessageReason = checkFormat(m)

  /**
   * Message sender should be present and have non zero stake in map.
   */
  def inactiveSender[M, S](m: M)(bondsMap: M => Map[S, Long], sender: M => S): Boolean =
    !bondsMap(m).exists { case (s, stake) => sender(m) == s && stake > 0 }

  /**
   * Message should have justification for each senders in the bonds map (both active and inactive).
   */
  def invalidJustificationFollows[M, S](m: M, bondedSenders: Set[S])(
      justifications: M => Set[M],
      sender: M => S
  ): Boolean                                                                           =
    justifications(m).map(sender) != bondedSenders

  /**
   * Message should have sequence number equal to sequence number of self justification + 1.
   * [[incompleteJustificationSet]] guarantees that self justification s present.
   */
  def invalidSequenceNumber[M](m: M, selfJustification: M)(seqNum: M => Long): Boolean =
    seqNum(m) != seqNum(selfJustification) + 1

  /**
   * Message should have block number equal to the block number of the highest justification + 1.
   */
  def invalidBlockNumber[M](m: M)(justifications: M => Set[M], blockNum: M => Long): Boolean =
    blockNum(m) != justifications(m).map(blockNum).max + 1

  /**
   * Message should not have justifications past to justification of previous message from the same sender.
   */
  def invalidJustificationRegression[M, S](
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
   * Message should not be potentially conflicting with finalized state (i.e. not `lazy`).
   *
   * Message M is lazy if it uses justifications below finalization fringe of the local view
   * AND messages of a finalization fringe do not see M (use M OR descendant as justification).
   */
  def invalidLazyMessage[M, S](
      message: M,
      dg: DependencyGraph[M, S],
      finalizationFringe: FinalizationFringe[M, S]
  )(seqNum: M => Long): Boolean =
    dg.justifications(message)
      .toIterator
      .flatMap { jm =>
        val s = dg.sender(jm)
        finalizationFringe.v.find {
          case (sf, mf) if s == sf =>
            val belowFringe = seqNum(mf) > seqNum(jm)
            val unseen      = !dg.justifications(mf).exists { x =>
              x == message ||
              (dg.sender(x) == dg.sender(message) &&
                dg.selfJustificationChain(x).find(_ == message).compile.last.nonEmpty)
            }
            belowFringe && unseen
        }
      }
      .nonEmpty

  /**
   * State transition exposed by sender in a message does not correspond to state transition computed from
   * justifications.
   */
  def invalidStateTransition[F[_], M, U](message: M, computedState: StateMessage[U])(
      state: M => StateMessage[U]
  ): Boolean = computedState != state(message)

  /**
   * For all validation functions above in this file it is assumed that message creator put real bonds map in a message.
   * But as sender might cheat, bonds map should be read directly from the state of the blockchain. This process
   * is out of the scope of Casper protocol, so validation rule is not aware of how exactly real blonds map is obtained.
   */
  def invalidBondsCache[S](messageBonds: Map[S, Long], realBonds: Map[S, Long]): Boolean =
    messageBonds != realBonds

  trait MalformedMessageReason
}

// TODO given creating messages only when there is a state change, validation of timestamp does not make sense. Remove at all?
//  val TIMESTAMP_TOLERANCE = 15.seconds
//
//  /**
//   * Message should have a timestamp. Ths provides a (quite relaxed) notion of a time on chain.
//   * Finalization fringe messages can give a answer on whether certain time point has passed.
//   */
//  def invalidTimestamp[F[_]: Applicative: Time, M](m: M, parents: Set[M])(
//      timestamp: M => Long
//  ): F[Boolean] =
//    Time[F].currentMillis.map { now =>
//      val allowedRange = (parents.map(timestamp).max, now + TIMESTAMP_TOLERANCE.toMillis)
//      val ts           = timestamp(m)
//      (allowedRange._1 < ts) && (ts < allowedRange._2)
//    }
