package coop.rchain.rholang.interpreter.storage

import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.models._
import coop.rchain.rholang.interpreter.PrettyPrinter
import coop.rchain.rholang.interpreter.implicits._
import coop.rchain.storage.IStore
import coop.rchain.storage.internal.{Datum, Row, WaitingContinuation}

object StoragePrinter {
  def prettyPrint(store: IStore[Channel, List[Channel], List[Channel], Par]): Unit = {
    val pars: Seq[Par] = store.toMap.map {
      case ((channels: List[Channel], row: Row[List[Channel], List[Channel], Par])) => {
        def toSends(data: List[Datum[List[Channel]]]): Par = {
          val sends: Seq[Send] = data.flatMap {
            case Datum(as: List[Channel], persist: Boolean) =>
              channels.map { channel =>
                Send(Some(channel), as.map { case Channel(Quote(p)) => p }, persist)
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
          case Row(Nil, Nil) =>
            Par()
          case Row(data: List[Datum[List[Channel]]], Nil) =>
            toSends(data)
          case Row(Nil, wks: List[WaitingContinuation[List[Channel], Par]]) =>
            toReceive(wks)
          case Row(data: List[Datum[List[Channel]]],
                   wks: List[WaitingContinuation[List[Channel], Par]]) =>
            toSends(data) ++ toReceive(wks)
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
      println(PrettyPrinter().buildString(par))
    }
  }
}
