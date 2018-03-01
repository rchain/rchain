package coop.rchain.rholang.interpreter

import monix.eval.Task

import scala.collection.immutable.HashMap

object Env {

  def empty = HashMap.empty[Int, Par]

  implicit class DeBruijn(env: Env[Par]) {

    def get(level: Int): Option[Par] = env.get(level)

    def put(level: Int)(data: Par): Env[Par] = env + (level -> data)

    /* Since indexing starts from 0, # of bound variables = level of
    environment associated with the term.*/
    def level: Int = env.keys.max + 1

    def rename(j: Int): Env[Par] = env map { case (k, data) => (k + j, data) }

    /* In [env] allocate [k.size] new slots */
    def alloc(k: List[Par]): Task[Env[Par]] = Task now (env /: k) { (_env, data) =>
      _env.put(_env.level)(data)
    }

    def merge(_env: Env[Par]): Env[Par] = env ++: _env.rename(env.level)

  }

}
