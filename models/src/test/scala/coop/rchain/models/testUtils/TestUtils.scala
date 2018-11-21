package coop.rchain.models.testUtils
import coop.rchain.models.Par
import coop.rchain.models.rholang.sorter.Sortable
import monix.eval.Coeval

object TestUtils {
  def sort(par: Par): Par = Sortable[Par].sortMatch[Coeval](par).map(_.term).value()
}
