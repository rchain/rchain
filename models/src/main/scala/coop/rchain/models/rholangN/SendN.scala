package coop.rchain.models.rholangN

final class SendN(val chan: ParN, val data: Seq[ParN], val persistent: Boolean) extends ParN

object SendN {
  def apply(chan: ParN, data: Seq[ParN], persistent: Boolean): SendN =
    new SendN(chan, data, persistent)

  def apply(chan: ParN, data: Seq[ParN]): SendN =
    apply(chan, data, persistent = false)

  def apply(chan: ParN, data: ParN, persistent: Boolean): SendN =
    apply(chan, Seq(data), persistent)

  def apply(chan: ParN, data: ParN): SendN =
    apply(chan, Seq(data), persistent = false)
}
