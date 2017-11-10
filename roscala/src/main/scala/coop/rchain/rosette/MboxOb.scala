package coop.rchain.rosette

import scala.collection.mutable

case class MboxOb(override val _slot: mutable.Seq[Ob]) extends Ob
