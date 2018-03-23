package coop.rchain.rholang.interpreter.storage

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models._
import coop.rchain.storage.InMemoryStore
import coop.rchain.storage.internal.{Datum, Row, WaitingContinuation}

import coop.rchain.rholang.interpreter.implicits._

object StoragePrinter {
  // TODO: Swap InMemoryStore with IStore
  def prettyPrint(store: InMemoryStore[Channel, List[Channel], List[Quote], Par]): Unit = {
    val pars: Seq[Par] = store.toHashMap.map {
      case ((channels: List[Channel], row: Row[List[Channel], List[Quote], Par])) => {
        def toSends(data: List[Datum[List[Quote]]]): Par = {
          val sends: Seq[Send] = data.flatMap {
            case Datum(as: List[Quote], persist: Boolean) =>
              channels.map { channel =>
                Send(Some(channel), as.map { case Quote(p) => p }, persist)
              }
          }
          sends.foldLeft(Par()) { (acc: Par, send: Send) =>
            acc.prepend(send)
          }
        }

        def toReceive(wks: List[WaitingContinuation[List[Channel], Par]]): Par = {
          val receives: Seq[Receive] = wks.map {
            case WaitingContinuation(patterns: List[List[Channel]],
                                     continuation: Par,
                                     persist: Boolean) =>
              val receiveBinds: Seq[ReceiveBind] = (channels zip patterns).map {
                case (channel, pattern) =>
                  ReceiveBind(pattern, Some(channel))
              }
              Receive(receiveBinds, Some(continuation), persist)
          }
          receives.foldLeft(Par()) { (acc: Par, receive: Receive) =>
            acc.prepend(receive)
          }
        }

        row match {
          case Row(Some(data: List[Datum[List[Quote]]]),
                   Some(wks: List[WaitingContinuation[List[Channel], Par]])) =>
            toSends(data) ++ toReceive(wks)
          case Row(Some(data: List[Datum[List[Quote]]]), None) =>
            toSends(data)
          case Row(None, Some(wks: List[WaitingContinuation[List[Channel], Par]])) =>
            toReceive(wks)
          case Row(None, None) =>
            Par()
        }
      }
    }.toList
    if (pars.isEmpty) {
      println(
        "The store is empty. Note that top level terms that are not sends or receives are discarded.")
    } else {
      val par = pars.reduce { (p1: Par, p2: Par) =>
        p1 ++ p2
      }
      PrettyPrinter.prettyPrint(par)
    }
  }
}
