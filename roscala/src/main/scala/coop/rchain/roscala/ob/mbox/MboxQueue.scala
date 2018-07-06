package coop.rchain.roscala.ob.mbox

import coop.rchain.roscala.ob.{Queue, Tuple}

import scala.collection.mutable

class MboxQueue(elems: Tuple) extends Queue(mutable.Queue(elems.value: _*))

object MboxQueue {
  def apply(): MboxQueue = new MboxQueue(Tuple())
}
