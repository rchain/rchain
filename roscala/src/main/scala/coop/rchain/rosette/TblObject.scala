package coop.rchain.rosette

import scala.collection.mutable

case class TblObject(entry: Seq[Ob], override val _slot: mutable.Seq[Ob])
    extends Ob

object TblObject {
  object PLACEHOLDER extends TblObject(null, null)
}
