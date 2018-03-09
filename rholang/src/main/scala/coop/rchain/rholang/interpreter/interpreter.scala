package coop.rchain.rholang

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Par

import scala.collection.immutable.HashMap

package object interpreter {

  type Data = Either[Quote, Par]

  type Env[A] = HashMap[Int, A]

}
