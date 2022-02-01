package coop.rchain.sdk.simulation.data

import cats.Monad
import cats.syntax.all._

import scala.collection.immutable.SortedMap

object Finalization02 {
  def showMsgs(ms: Seq[Msg]) =
    ms.sortBy(x => (x.height, x.id)).map(_.id).mkString(" ")

  // Sender represents validator node
  final case class Sender(id: Int, stake: Int) {
    override def hashCode(): Int = this.id.hashCode()
  }

  // Message exchanged between senders (validators)
  final case class Msg(
      id: String,
      height: Int,
      sender: Sender,
      senderSeq: Int,
      justifications: Map[Int, String]
  ) {
    override def hashCode(): Int = this.id.hashCode()
  }

  // M |- root{ seenMsgs Final{ finalized } }
  final case class MsgView(
      root: Msg,
      seenMsgs: Set[MsgView],
      finalized: Set[Msg]
  ) {
    override def hashCode(): Int = this.root.id.hashCode()

    override def toString: String = {
      val sms     = seenMsgs.map(_.root)
      val seenStr = showMsgs(sms.toSeq)

      val finalizedStr = showMsgs(finalized.toSeq)

      s"${root.id} { $seenStr F{ $finalizedStr }}"
    }
  }

  // SenderState represents state of one validator in the network
  final case class SenderState(
      me: Sender,
      seqNum: Int,
      latestMsgs: Map[Sender, Msg],
      dag: Map[String, Msg],
      heightMap: SortedMap[Int, Set[Msg]],
      // Message view
      // - updated when new message is added
      seen: Map[Msg, MsgView] = Map()
  ) {
    override def hashCode(): Int = this.me.id.hashCode()

    def addMsg(msg: Msg): SenderState =
      if (dag.contains(msg.id)) this
      else {
        // Find latest message for sender
        val latest = latestMsgs(msg.sender)

        // Update latest message if newer
        val latestFromSender = msg.senderSeq > latest.senderSeq
        val newLatestMsgs =
          if (latestFromSender) latestMsgs + ((msg.sender, msg))
          else latestMsgs

        if (!latestFromSender)
          println(s"ERROR: add NOT latest message '${msg.id}' for sender '${me.id}''")

        // Update DAG
        val newDag = dag + ((msg.id, msg))

        // Update seqNum if sender's state
        val newSeqNum = if (me == msg.sender && msg.senderSeq > seqNum) msg.senderSeq else seqNum

        // Update height map
        val heightSet    = heightMap.getOrElse(msg.height, Set())
        val newHeightMap = heightMap + ((msg.height, heightSet + msg))

        // Update seen messages for message sender
        def loadJfs(m: Msg) = m.justifications.map { case (_, mid) => newDag(mid) }.toSet
        def loadMsgViews(m: Msg) =
          m.justifications.map { case (_, mid) => newDag(mid) }.map(seen(_)).toSet

        // Traverse message justifications to update seen messages
        val msgJfs      = loadJfs(msg)
        val msgViewsJfs = loadMsgViews(msg)
        val iterSeen = msgJfs.map { rootMsg =>
          val seerJfs   = loadJfs(rootMsg) // -- msgView.finalized
          val seerViews = seerJfs.map(seen)
          rootMsg.sender -> Iterator
            .iterate((seerViews, seerViews)) {
              case (acc, jfs) =>
                val toAdd = jfs.flatMap(x => loadMsgViews(x.root)) // -- msgView.finalized
                val next  = toAdd -- acc
                //                println(s"ACC ${acc.map(_.id)}")
                //                println(s"NEXT ${next.map(_.id)}")
                (acc ++ next, next)
            }
            .takeWhile(_._2.nonEmpty)
            .flatMap(_._2)
            .toSet
        }.toMap

        val selfParent              = msgViewsJfs.find(x => x.root.sender == msg.sender)
        def hasSeenInParent(m: Msg) =
          //          selfParent.exists(x => x.root.justifications.values.exists(_ == m.id))
          selfParent.exists(x => loadMsgViews(x.root).exists(x => x.root.id == m.id))
        val mViews = msgViewsJfs.filterNot { m =>
          hasSeenInParent(m.root) || m.finalized.contains(m.root)
        }

        val seenBySeen = mViews
          .foldLeft(Map[Msg, Set[MsgView]]()) {
            case (acc, mv) =>
              val curSeen = acc.getOrElse(mv.root, Set[MsgView]())
              val newSeen = curSeen ++ mv.seenMsgs
              acc + ((mv.root, newSeen))
          }
          .filter(_._2.nonEmpty)

        val seers     = seenBySeen.keySet.map(_.sender)
        val partition = seenBySeen.flatMap(x => x._2.filter(y => seers(y.root.sender)))

        val seenBySeenStr = seenBySeen.toList
          .sortBy(_._1.id)
          .map {
            case (m, mvs) =>
              val msgs = mvs.map(_.root)
              s"  ${m.id} ${showMsgs(msgs.toSeq)}"
          }
          .mkString("\n")

        val mFinalized = if (partition.size >= 2) {
          partition.map(_.root).toSet
        } else {
          msgViewsJfs.flatMap(_.finalized)
        }

        val newMsgView = MsgView(root = msg, seenMsgs = mViews, finalized = mFinalized)

        val newSeen = seen + ((msg, newMsgView))

        val msgSeenByStr = iterSeen
          .map {
            case (s, mvs) =>
              //              val mvsStr = mvs.mkString(", ")
              val mvsStr = showMsgs(mvs.map(_.root).toSeq)
              s"  ${s.id} ⊢ $mvsStr"
          }
          .mkString("\n")

        if (me == msg.sender) {
          println(s"${me.id}: ADDED ${msg.id}")
          //          println(s"DAG ${showMsgs(newDag.values.toSeq)}")
          println(s"COMM:\n$seenBySeenStr")
          println(s"VIEW:\n  $newMsgView")
          //          println(s"SEEN:\n$msgSeenByStr")
          println(s"---------------------------------")
        }

        copy(
          seqNum = newSeqNum,
          latestMsgs = newLatestMsgs,
          dag = newDag,
          heightMap = newHeightMap,
          seen = newSeen
        )
      }

    def createMsg(): (SenderState, Msg) = {
      val maxHeight      = latestMsgs.map(_._2.height).max
      val newHeight      = maxHeight + 1
      val newSeqNum      = seqNum + 1
      val justifications = latestMsgs.map { case (s, m) => (s.id, m.id) }

      // Create new message
      val newMsg = Msg(
        id = s"${me.id}-$newHeight",
        height = newHeight,
        sender = me,
        senderSeq = newSeqNum,
        justifications
      )

      // Add message to self state
      val newState = addMsg(newMsg)

      (newState, newMsg)
    }
  }

  // State that represents the whole network
  final case class Network(senders: Set[SenderState]) {
    // Split network
    def split(perc: Float) = {
      val total = senders.size
      val first = Math.round(total * perc)
      senders.splitAt(first).bimap(Network, Network)
    }

    // Merge networks
    def >|<(that: Network) = {
      val (s1, s2) = (senders, that.senders)
      // Exchange messages
      val msgs1 = s1.flatMap(_.dag.values)
      val msgs2 = s2.flatMap(_.dag.values)

      val newSenders1 =
        s1.map(s => msgs2.toList.sortBy(x => x.height).foldLeft(s)((acc, m) => acc.addMsg(m)))
      val newSenders2 =
        s2.map(s => msgs1.toList.sortBy(x => x.height).foldLeft(s)((acc, m) => acc.addMsg(m)))

      Network(newSenders1 ++ newSenders2)
    }
  }

  def initNetwork(sendersCount: Int, stake: Int) = {
    // Arbitrary number of senders (bonded validators)
    val senders = (0 until sendersCount).map { n =>
      Sender(n, stake)
    }.toSet

    // Genesis message created by first sender
    val sender0 = senders.find(_.id == 0).get
    val genesisMsg =
      Msg(s"g", height = 0, sender = sender0, senderSeq = -1, justifications = Map())

    // Latest messages for all senders is genesis message
    val latestMsgs = senders.map((_, genesisMsg)).toMap

    // Initial messages in the DAG
    val dag = Map((genesisMsg.id, genesisMsg))

    val heightMap = SortedMap(genesisMsg.height -> Set(genesisMsg))

    // Seen state
    val seen = Map(
      genesisMsg -> MsgView(root = genesisMsg, seenMsgs = Set(), finalized = Set(genesisMsg))
    )

    val senderStates =
      senders.map(s => SenderState(me = s, seqNum = 0, latestMsgs, dag, heightMap, seen))

    Network(senderStates)
  }

  def runNetwork[F[_]: Monad](network: Network, genHeight: Int, skipPercentage: Float) =
    (genHeight, network).tailRecM {
      case (round, net) =>
        //        println(s"ROUND: $round")

        val newMsgSenders = net.senders.map { ss =>
          val rnd = Math.random()
          val (newS, m) =
            if (rnd > skipPercentage) ss.createMsg()
            else (ss, ss.dag.head._2)
          (newS, m)
        }

        //        println(s"  > created msg")

        val newSS   = newMsgSenders.map(_._1)
        val newMsgs = newMsgSenders.map(_._2)

        val newSenderStates = newMsgs.foldLeft(newSS) {
          case (ss, m) =>
            ss.map(_.addMsg(m))
        }

        //        println(s"  > added msg")

        val newNet = net.copy(newSenderStates)

        val res =
          if (round > 1) (round - 1, newNet).asLeft // Loop
          else newNet.asRight                       // Final value
        res.pure[F]
    }
}
