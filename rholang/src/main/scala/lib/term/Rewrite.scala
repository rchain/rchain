// -*- mode: Scala;-*-
// Filename:    Rewrite.scala
// Authors:     luciusmeredith
// Creation:    Thu Feb  2 12:35:45 2017
// Copyright:   See site license
// Description:
// ------------------------------------------------------------------------

package coop.rchain.lib.term

import coop.rchain.lib.zipper._
import coop.rchain.lib.navigation.{ Right => _, Left => _, _ }

trait TermNavigation[L,V,T] extends ZipperNavigation[Either[T,V]]
trait TermMutation[L,V,T] extends ZipperMutation[Either[T,V]]

trait TermZipperComposition[L,V,T] {
  // Composing a context places the inner context in the hole of the outer
  // context, leaving a larger context with a single hole.
  def compose(
    ctxtInner : Context[Either[T,V]],
    ctxtOuter : Context[Either[T,V]]
  ) : Context[Either[T,V]] = {
    ctxtInner match {
      case Top() => ctxtOuter
      case LabeledTreeContext(lbl: L @unchecked, left: List[TermCtxt[L, V, T] with Factual] @unchecked, ctxt: LabeledTreeContext[L, Either[T, V]] @unchecked, right: List[TermCtxt[L, V, T] with Factual] @unchecked) => {
	LabeledTreeContext[L,Either[T,V]](
	  lbl, left, compose( ctxt, ctxtOuter ), right
	)
      }
    }
  }
}
