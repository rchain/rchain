
import coop.rchain.lib.zipper._
import coop.rchain.lib.navigation.{Left => _, Right => _, _}
import coop.rchain.lib.term.{Factual, TagOrVar, TermCtxt}

trait TermNavigation[L,V,T] extends ZipperNavigation[TagOrVar[T,V]]
trait TermMutation[L,V,T] extends ZipperMutation[TagOrVar[T,V]]

trait TermZipperComposition[L,V,T] {
  // Composing a context places the inner context in the hole of the outer
  // context, leaving a larger context with a single hole.
  def compose(
               ctxtInner : Context[TagOrVar[T,V]],
               ctxtOuter : Context[TagOrVar[T,V]]
             ) : Context[TagOrVar[T,V]] = {
    ctxtInner match {
      case Top() => ctxtOuter
      case LabeledTreeContext(lbl: L @unchecked, left: List[TermCtxt[L, V, T] with Factual] @unchecked, ctxt: LabeledTreeContext[L, TagOrVar[T, V]] @unchecked, right: List[TermCtxt[L, V, T] with Factual] @unchecked) => {
        LabeledTreeContext[L,TagOrVar[T,V]](
          lbl, left, compose( ctxt, ctxtOuter ), right
        )
      }
    }
  }
}
