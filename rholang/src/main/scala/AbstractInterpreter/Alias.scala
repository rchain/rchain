import ADT.{Channel, Proc}
import AbstractInterpreter.ChannelQueue
import cats.data.{StateT, WriterT}

import scala.collection.immutable.HashMap

package object State {

  type Trace[S, W, A] = StateT[WriterT[List, W, ?], S, A]

  /** The store is a finite partial mapping from Channels to ChannelQueues. */
  type Store = HashMap[Channel, ChannelQueue]

  /** The run-queue is just the list of expressions to be evaluated. */
  type RunQueue = List[Proc]

}
