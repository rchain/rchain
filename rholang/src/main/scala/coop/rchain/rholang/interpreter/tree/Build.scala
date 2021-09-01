package coop.rchain.rholang.interpreter.tree

import coop.rchain.rholang.interpreter.tree.instances.TreeBuilder

trait Build[S, A] {
  def normalize(p: S, parent: Option[A]): A
}

object Build extends TreeBuilder {
  def apply[S, A](implicit ev: Build[S, A]): Build[S, A] = ev
}
