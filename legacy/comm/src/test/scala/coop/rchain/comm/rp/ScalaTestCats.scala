package coop.rchain.comm.rp

import cats.Id

import org.scalatest.enablers.Containing

object ScalaTestCats {
  implicit def idContaining[C](implicit C: Containing[C]): Containing[Id[C]] =
    new Containing[Id[C]] {
      def contains(container: cats.Id[C], element: Any): Boolean = {
        val con: C = container
        C.contains(con, element)
      }
      def containsNoneOf(container: cats.Id[C], elements: Seq[Any]): Boolean = {
        val con: C = container
        C.containsNoneOf(con, elements)
      }
      def containsOneOf(container: cats.Id[C], elements: Seq[Any]): Boolean = {
        val con: C = container
        C.containsOneOf(con, elements)
      }
    }
}
