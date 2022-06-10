package coop.rchain.blockstorage.dag

import cats.syntax.all._

object DagMessageState {
  def apply[M: Ordering, S: Ordering](
      latestMsgs: Set[Message[M, S]]
  ): DagMessageState[M, S] = new DagMessageState(latestMsgs, Map())
}

// DagMessageState represents state of one validator in the network
final case class DagMessageState[M: Ordering, S: Ordering](
    latestMsgs: Set[Message[M, S]],
    // Message - updated when new message is added
    msgMap: Map[M, Message[M, S]]
) {

  /**
    * Creates a new message, generates id (hash) and finalization fringe
    */
  def createMessage(
      id: M,
      height: Long,
      sender: S,
      senderSeq: Long,
      finBondsMap: Map[S, Long],
      justifications: Set[Message[M, S]]
  ): Message[M, S] = {
    // Calculate next fringe or continue with parent
    val finalizer                    = Finalizer(msgMap)
    val (parentFringe, newFringeOpt) = finalizer.calculateFinalization(justifications, finBondsMap)

    val newFringe    = newFringeOpt.getOrElse(parentFringe)
    val newFringeIds = newFringe.map(_.id)

    // Seen messages are all seen from justifications combined
    val seenByParents = justifications.flatMap(_.seen)
    val newSeen       = seenByParents + id

    // Create message, an immutable object with all fields calculated
    val justificationKeys = justifications.map(_.id)
    val newMsg =
      Message(
        id,
        height,
        sender,
        senderSeq,
        finBondsMap,
        justificationKeys,
        fringe = newFringeIds,
        seen = newSeen
      )

    /* Debug log */
    // val fringeStr = newFringeOpt.map(ms => s"+ ${showMsgs(ms)}").getOrElse("")
    // println(s"${newMsg.id} $fringeStr")
    debugLogMessage(finalizer, newMsg, justifications, parentFringe, newFringeOpt)
    /* End Debug log */

    newMsg
  }

  /**
    * Inserts a message to sender's state
    */
  def insertMsg(msg: Message[M, S]): DagMessageState[M, S] =
    msgMap.get(msg.id).as(this).getOrElse {
      // Add message to a message map
      val newMsgMap = msgMap + ((msg.id, msg))

      // Find latest message for sender
      val latest = latestMsgs.filter(_.sender == msg.sender)

      // Update latest messages for sender
      val latestFromSender = msg.senderSeq > latest
        .map(_.senderSeq)
        .toList
        .maximumOption
        .getOrElse(-1L)
      val newLatestMsgs =
        if (latestFromSender) latestMsgs -- latest + msg
        else latestMsgs

      // TODO: temp disabled until working finalizer
//      if (!latestFromSender)
//        println(s"ERROR: add NOT latest message '${msg.id}' for sender '$me''")

      // Create new sender state with added message
      copy(latestMsgs = newLatestMsgs, msgMap = newMsgMap)
    }

  /**
    * Creates a new message and adds it to sender state
    */
  def createMsgAndUpdateSender(
      creator: S,
      genMsgId: (S, Long) => M
  ): (DagMessageState[M, S], Message[M, S]) = {
    val maxHeight      = latestMsgs.map(_.height).max
    val newHeight      = maxHeight + 1
    val seqNum         = latestMsgs.find(_.sender == creator).map(_.senderSeq).getOrElse(0L)
    val newSeqNum      = seqNum + 1
    val justifications = latestMsgs
    // Bonds map taken from any latest message (assumes no epoch change happen)
    val bondsMap = latestMsgs.head.bondsMap

    // Generate message ID (to be updated with real block hash)
    val msgId = genMsgId(creator, newHeight)

    // Create a new message
    val newMsg = createMessage(
      id = msgId,
      height = newHeight,
      sender = creator,
      senderSeq = newSeqNum,
      bondsMap,
      justifications
    )

    // Insert message to self state
    (insertMsg(newMsg), newMsg)
  }

  /**
    * DEBUG: Prints debug output of a message (it has overhead of fringe support Map calculation)
    */
  def debugLogMessage(
      finalizer: Finalizer[M, S],
      msg: Message[M, S],
      justifications: Set[Message[M, S]],
      parentFringe: Set[Message[M, S]],
      newFringeOpt: Option[Set[Message[M, S]]]
  ): Unit = {
    def printNextFringeSupportMap(fringeSupportMap: Map[S, Map[S, Set[S]]]) =
      fringeSupportMap.toList
        .sortBy(_._1)
        .map {
          case (sp, seenByMap) =>
            val seenMapStr = seenByMap.toList
              .sortBy(_._1)
              .map {
                case (s, ss) =>
                  val ssStr = ss.toList.sorted.mkString(", ")
                  s"$s($ssStr)"
              }
              .mkString(" ")
            s"  $sp: $seenMapStr"
        }
        .mkString("\n")

    val debugInfo = for {
      // Find minimum message from each sender from justifications
      minMsgs <- justifications.toList.traverse(
                  p => (p +: finalizer.selfParents(p, parentFringe)).lastOption
                )

      // Check if min messages satisfy requirements (senders in bonds map)
      _ <- finalizer.checkMinMessages(minMsgs, msg.bondsMap).guard[Option]

      // Include ancestors of minimum messages as next layer
      nextLayer = finalizer.calculateNextLayer(minMsgs)

      // Create next fringe support Map map for each justification (sender)
      fringeSupportMap = finalizer.calculateNextFringeSupportMap(
        justifications,
        nextLayer,
        parentFringe
      )

      // Debug print
      minMsgsStr       = showMsgs(minMsgs)
      nextLayerStr     = showMsgs(nextLayer.values.toSeq)
      fringeSupportStr = printNextFringeSupportMap(fringeSupportMap)
    } yield (nextLayerStr, fringeSupportStr, minMsgsStr)

    val (nextLayerStr, fringeSupportStr, minMsgsStr) = debugInfo.getOrElse(("", "", ""))
    val (prefix, fringe)                             = newFringeOpt.map(("+", _)).getOrElse((":", parentFringe))
    val fringeStr                                    = showMsgs(fringe.toSeq)
    val parentFringeStr                              = showMsgs(parentFringe.toSeq)

    val printOutputs = Seq(nextLayerStr, fringeSupportStr, minMsgsStr, fringeStr, parentFringeStr)
    if (printOutputs.exists(_ != "")) {
      println(s"${msg.sender}: ${msg.id}")
      println(s"SUPPORT:\n$fringeSupportStr")
      println(s"MIN    : $minMsgsStr")
      println(s"NEXT   : $nextLayerStr")
      println(s"PREV_F : $parentFringeStr")
      println(s"FRINGE $prefix $fringeStr")
      println(s"---------------------------------")
    }
  }

  def showMsgs(ms: Seq[Message[M, S]]): String =
    ms.sortBy(x => (x.sender, x.height, x.id)).map(_.id).mkString(" ")
}
