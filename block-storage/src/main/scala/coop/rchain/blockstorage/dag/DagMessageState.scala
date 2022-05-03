package coop.rchain.blockstorage.dag

import cats.syntax.all._

// DagMessageState represents state of one validator in the network
final case class DagMessageState[M: Ordering, S: Ordering](
    me: S,
    latestMsgs: Set[Message[M, S]],
    // Message view - updated when new message is added
    msgViewMap: Map[M, Message[M, S]] = Map()
) {

  /**
    * Creates a new message, generates id (hash) and finalization fringe
    */
  def createMessageView(
      id: M,
      height: Long,
      sender: S,
      senderSeq: Long,
      finBondsMap: Map[S, Long],
      justifications: Set[Message[M, S]]
  ): Message[M, S] = {
    // Calculate next fringe or continue with parent
    val finalizer                    = Finalizer(msgViewMap)
    val (parentFringe, newFringeOpt) = finalizer.calculateFinalization(justifications, finBondsMap)

    val newFringe    = newFringeOpt.getOrElse(parentFringe)
    val newFringeIds = newFringe.map(_.id)

    // Seen messages are all seen from justifications combined
    val seenByParents = justifications.flatMap(_.seen)
    val newSeen       = seenByParents + id

    // Create message view, an immutable object with all fields calculated
    val justificationKeys = justifications.map(_.id)
    val newMsgView =
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
    if (me == sender) {
      //        val fringeStr = newFringeOpt.map(ms => s"+ ${showMsgs(ms)}").getOrElse("")
      //        println(s"${newMsgView.id} $fringeStr")
      debugLogMessage(finalizer, newMsgView, justifications, parentFringe, newFringeOpt)
    }
    /* End Debug log */

    newMsgView
  }

  /**
    * Inserts a message to sender's state
    */
  def insertMsgView(msgView: Message[M, S]): DagMessageState[M, S] =
    msgViewMap.get(msgView.id).as(this).getOrElse {
      // Add message view to a view map
      val newMsgViewMap = msgViewMap + ((msgView.id, msgView))

      // Find latest message for sender
      val latest = latestMsgs.filter(_.sender == msgView.sender)

      // Update latest messages for sender
      val latestFromSender = msgView.senderSeq > latest
        .map(_.senderSeq)
        .toList
        .maximumOption
        .getOrElse(-1L)
      val newLatestMsgs =
        if (latestFromSender) latestMsgs -- latest + msgView
        else latestMsgs

      if (!latestFromSender)
        println(s"ERROR: add NOT latest message '${msgView.id}' for sender '$me''")

      // Create new sender state with added message
      copy(latestMsgs = newLatestMsgs, msgViewMap = newMsgViewMap)
    }

  /**
    * Creates a new message and adds it to sender state
    */
  def createMsgAndUpdateSender(genMsgId: (S, Long) => M): (DagMessageState[M, S], Message[M, S]) = {
    val maxHeight      = latestMsgs.map(_.height).max
    val newHeight      = maxHeight + 1
    val seqNum         = latestMsgs.find(_.sender == me).map(_.senderSeq).getOrElse(0L)
    val newSeqNum      = seqNum + 1
    val justifications = latestMsgs
    // Bonds map taken from any latest message (assumes no epoch change happen)
    val bondsMap = latestMsgs.head.bondsMap

    // Create a message view from a new received message
    val msgId = genMsgId(me, newHeight) // s"$me-$height"

    // Create new message
    val newMsg = createMessageView(
      id = msgId,
      height = newHeight,
      sender = me,
      senderSeq = newSeqNum,
      bondsMap,
      justifications
    )

    // Insert message view to self state
    (insertMsgView(newMsg), newMsg)
  }

  /**
    * DEBUG: Prints debug output of a message (it has overhead of fringe support Map calculation)
    */
  def debugLogMessage(
      finalizer: Finalizer[M, S],
      msgView: Message[M, S],
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
      _ <- finalizer.checkMinMessages(minMsgs, msgView.bondsMap).guard[Option]

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
      println(s"${me}: ${msgView.id}")
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
