package coop.rchain.rosette.utils.opcodes

import shapeless.{::, ops, DepFn1, HList, HNil, LabelledGeneric, Witness}

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Can not derive instance for DeepSelector. Possibly wrong path is specified.")
trait DeepSelector[K <: HList, T] extends DepFn1[T]
object DeepSelector {
  type WrList[A] = (List[Witness], A)
  type Aux[K1 <: HList, T1, Out0] = DeepSelector[K1, T1] { type Out = Out0 }

  abstract class Impl[Path <: HList, A, B] extends DeepSelector[Path, A] {
    type Out = B
  }

  def apply[Path <: HList, A](
      implicit sel: DeepSelector[Path, A]): Aux[Path, A, sel.Out] = sel

  implicit def nil[A]: Impl[HNil, A, WrList[A]] = x => (Nil, x)

  implicit def cons[K, Path <: HList, A, Repr <: HList, I, B](
      implicit wit: Witness.Aux[K],
      lgen: LabelledGeneric.Aux[A, Repr],
      select: ops.record.Selector.Aux[Repr, K, I],
      rest: Aux[Path, I, WrList[B]]): Impl[K :: Path, A, WrList[B]] = { x =>
    val (l, v) = rest(select(lgen.to(x)))
    (wit :: l, v)
  }
}
