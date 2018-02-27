package coop.rchain.rholang

import scala.collection.immutable.HashMap

package object interpreter {

  type Pattern = List[Channel]

  type Data = Either[Quote, Par]

  type Env = HashMap[Int, Data]

}
