package coop.rchain.rholang

import scala.collection.immutable.HashMap

package object interpreter {

  type Data = Either[Quote, Par]

  type Env[A] = HashMap[Int, A]

}
