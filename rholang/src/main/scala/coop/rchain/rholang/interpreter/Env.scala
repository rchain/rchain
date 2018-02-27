package coop.rchain.rholang.interpreter

object Env {

  implicit class DeBruijn(env: Env) {

    def get(level: Int): Option[Data] = env.get(level)

    def put(level: Int)(data: Data): Env = env + (level -> data)

    /* Since indexing starts from 0, # of bound variables = level of
    environment associated with the term.*/
    def level: Int = env.keys.max + 1

    def rename(j: Int): Env = env map { case (k, data) => (k + j, data) }

    /* In [env] allocate [k.size] new slots */
    def alloc(k: List[Data]): Env = (env /: k) { (_env, data) =>
      _env.put(_env.level)(data)
    }

    def merge(_env: Env): Env = env ++: _env.rename(env.level)

  }

}
