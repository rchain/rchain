package coop.rchain.rholang.interpreter

import coop.rchain.models._

import scala.collection.immutable.HashMap

/* Env reifies the Env[A] = HashMap[Int,A] type alias.
   It extends HashMap with functions that manipulate
   the map based on level indices. */

object Env {

  def empty = HashMap.empty[Int, Par]

  implicit class DeBruijn(env: HashMap[Int, Par]) {

    def get(level: Int): Option[Par] = env.get(level)

    def put(level: Int)(data: Par): HashMap[Int, Par] = env + (level -> data)

    /* Since indexing starts from 0, # of bound variables = level of
       environment associated with the term.*/
    def level: Int = env.keys.max + 1

    def rename(j: Int): HashMap[Int, Par] = env map { case (k, data) => (k + j, data) }

    /* In [env] allocate [k.size] new slots */
    def alloc(k: List[Par]): HashMap[Int, Par] = (env /: k) { (_env, data) =>
      _env.put(_env.level)(data)
    }

    def merge(_env: HashMap[Int, Par]): HashMap[Int, Par] = env ++: _env.rename(env.level)

  }
}
