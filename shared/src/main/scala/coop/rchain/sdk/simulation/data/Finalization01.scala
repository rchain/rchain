package coop.rchain.sdk.simulation.data

import cats.Monad
import cats.syntax.all._

import scala.collection.immutable.SortedMap

object Finalization01 {
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

  case class SeenBySender(
      seenMsgs: Map[Sender, Set[Msg]],
      finalized: Set[Msg],
      tempFinalized: Set[Msg] = Set()
  ) {
    override def toString: String = {
      val seenStr = seenMsgs.toList
        .sortBy(_._1.id)
        .map {
          case (s, ms) =>
            val msStr = showMsgs(ms.toSeq)
            s"  ${s.id}: $msStr"
        }
        .mkString("\n")

      val finalizedStr = showMsgs(finalized.toSeq)

      s"VIEW:\n$seenStr\n  FIN: $finalizedStr"
    }
  }

  // SenderState represents state of one validator in the network
  final case class SenderState(
      me: Sender,
      seqNum: Int,
      latestMsgs: Map[Sender, Msg],
      dag: Map[String, Msg],
      heightMap: SortedMap[Int, Set[Msg]],
      // View of the seen messages for each sender
      // - updated when new message is added
      seen: Map[Sender, SeenBySender] = Map()
  ) {
    override def hashCode(): Int = this.me.id.hashCode()

    override def toString: String = {
      val latestMsg  = latestMsgs(me)
      val latestStr  = s"${latestMsg.id}"
      val senderSeen = seen(me)
      s"BY $latestStr $senderSeen"
    }

    def addMsg(m: Msg): SenderState =
      if (dag.contains(m.id)) this
      else {
        // Find latest message for sender
        val latest = latestMsgs(m.sender)

        // Update latest message if newer
        val latestFromSender = m.senderSeq > latest.senderSeq
        val newLatestMsgs =
          if (latestFromSender) latestMsgs + ((m.sender, m))
          else latestMsgs

        if (!latestFromSender)
          println(s"ERROR: add NOT latest message '${m.id}' for sender '${me.id}''")

        // Update DAG
        val newDag = dag + ((m.id, m))

        // Update seqNum if sender's state
        val newSeqNum = if (me == m.sender && m.senderSeq > seqNum) m.senderSeq else seqNum

        // Update height map
        val heightSet    = heightMap.getOrElse(m.height, Set())
        val newHeightMap = heightMap + ((m.height, heightSet + m))

        // Update seen messages for message sender
        def loadJfs(msg: Msg) = msg.justifications.map { case (_, mid) => newDag(mid) }.toSet

        val senderSeen = seen(m.sender)

        // Traverse message justifications to update seen messages
        val msgJfs = loadJfs(m)
        val iterSeen = msgJfs.map { seerMsg =>
          val seerJfs = loadJfs(seerMsg) -- senderSeen.finalized
          seerMsg.sender -> Iterator
            .iterate((seerJfs, seerJfs)) {
              case (acc, jfs) =>
                val toAdd = jfs.flatMap(loadJfs) -- senderSeen.finalized
                val next  = toAdd -- acc
//                println(s"ACC ${acc.map(_.id)}")
//                println(s"NEXT ${next.map(_.id)}")
                (acc ++ next, next)
            }
            .takeWhile(_._2.nonEmpty)
            .flatMap(_._2)
            .toSet
        }.toMap

//        val seenByMsgSender = iterSeen
//          .get(m.sender)
//          .map(x => x.toList.sortBy(x => (x.height, x.id)).map(_.id).mkString(" "))
//          .getOrElse("")
//        if (m.sender == me) {
//          println(s"ADDED ${m.id} - $seenByMsgSender")
//          val nonFinalizedStr = newNonFinalized.map(_.id).toList.sorted.mkString(" ")
//          println(s"NON_FIN - $nonFinalizedStr")
//        }

        val newSenderSeen = iterSeen
          .foldLeft(senderSeen) {
            case (acc, (s, msgs)) =>
              val curMsgs     = acc.seenMsgs(s)
              val newMsgs     = curMsgs ++ msgs
              val newSeenMsgs = acc.seenMsgs + ((s, newMsgs))
              acc.copy(seenMsgs = newSeenMsgs)
          }

//        val newSeen = (seen + ((m.sender, newSenderSeen))).mapValues { seenBy =>
//          val newNonFinalized = seenBy.nonFinalized + m
//          seenBy.copy(nonFinalized = newNonFinalized)
//        }
        val newSeen = seen + ((m.sender, newSenderSeen))

        // Finalization

        // Find next layer mutually seen
//        val next = newSeen.mapValues { seenBySender =>
//          // Take first for each sender
//          val nextLayer = seenBySender.seenMsgs.toList.flatMap { case (s, msgs) =>
//            msgs.headOption.map(x => (s, x)).toList
//          }
//          nextLayer
//        }.filter(x => x._2.nonEmpty)
//        val nextStr = next.map { case (s, ms) =>
//          (s.id, ms.map(x => (x._1.id, x._2.id)))
//        }

        // Take first for each sender
        val nextLayer = newSenderSeen.seenMsgs.map {
          case (s, msgs) =>
            val ss = msgs.groupBy(_.sender)
            val msgsNext = ss.flatMap {
              case (_, ms) =>
                ms.toList.sortBy(_.senderSeq).headOption.toList
            }
            (s, msgsNext.toSet)
        }

        val msgSeenBy = nextLayer.foldLeft(Map[Msg, Set[Sender]]()) {
          case (acc, (s, ms)) =>
            ms.foldLeft(acc) {
              case (a, m) =>
                val curSenders = a.getOrElse(m, Set[Sender]())
                val newSenders = curSenders + s
                a + ((m, newSenders))
            }
        }
        val msgSeenByStr = msgSeenBy
          .map {
            case (m, ss) =>
              val ssStr = ss.map(_.id).toSeq.sorted.mkString(" ")
              s"${m.id} $ssStr"
          }
          .mkString(", ")

        // Find common for at least two senders
        def hasCommon[A](s1: Set[A], s2: Set[A]) = (s1 | s2).size >= 2
        val allCommon = nextLayer.toList
          .combinations(2)
          .map {
            case List((s1, ms1), (s2, ms2)) =>
//              println(s"M1: $ms1, M2: $ms2")
              (s1, s2, ms1 & ms2)
          }
          .filter { case (_, _, ms) => ms.size >= 2 }
          .toVector

        val commonStr = allCommon
          .map {
            case (s1, s2, ms) =>
              val msStr = ms.toList.sortBy(x => x.sender.id).map(_.id).mkString(" ")
              s"${s1.id}:${s2.id} $msStr"
          }
          .mkString(", ")

        val nextStr = nextLayer.toList
          .sortBy(_._1.id)
          .map {
            case (s, ms) =>
              val msgsStr = ms.toList.sortBy(x => x.sender.id).map(_.id).mkString(" ")
              s"  ${s.id}: $msgsStr"
          }
          .mkString("\n")

        if (me == m.sender) {
          println(s"${me.id}: ADDED ${m.id}")
          val dagStr = showMsgs(newDag.values.toSeq)
          println(s"DAG $dagStr")
          //        println(s"NFIN ${showMsgs(newNonFinalized.toSeq)}")
          println(newSenderSeen)
          println(s"NEXT:\n$nextStr")
          println(s"COMM:\n$commonStr")
          println(s"S BY:\n$msgSeenByStr")
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

//    def receive(senderId: Int, msg: Msg) = {
//      val newSenders = senders.map { s =>
//        if (s.me.id == senderId) {
//          // Add message
//          s.addMsg(msg)
//        } else s
//      }
//      copy(senders = newSenders)
//    }
//
//    def receiveAll(msg: Msg) = {
//      val newSenders = senders.map(_.addMsg(msg))
//      copy(senders = newSenders)
//    }
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
    val seenBySender = senders.map(s => s -> Set[Msg]()).toMap
    val seen = senders
      .map(s => s -> SeenBySender(seenMsgs = seenBySender, finalized = Set(genesisMsg)))
      .toMap

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
