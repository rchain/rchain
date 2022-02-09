package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.ParBuilderUtil.assertCompiledEqual
import org.scalatest.{FlatSpec, Matchers}

class SynchronousOutputSpec extends FlatSpec with Matchers {

  "';'" should "have a higher precedence than '|' 2" in {
    val s = "@3!?(3). | @1!?(1) ; @2!?(2). "
    val t =
      """
        # new x0 in {
        #   @3!(*x0, 3) |
        #   for(_ <- x0){
        #     Nil
        #   }
        # } |
        # new x0 in {
        #   @1!(*x0, 1) |
        #   for(_ <- x0){
        #     new x1 in {
        #       @2!(*x1, 2) |
        #       for(_ <- x1 ) {
        #         Nil
        #       }
        #     }
        #   }
        # }
        # """.stripMargin('#')
    assertCompiledEqual(s, t)
  }

  "';'" should "have a higher precedence than '|'" in {
    val s = "@1!?(1) ; @2!?(2). | @3!?(3)."
    val t =
      """
          # new x0 in {
          #   @1!(*x0, 1) |
          #   for(_ <- x0){
          #     new x1 in {
          #       @2!(*x1, 2) |
          #       for(_ <- x1 ) {
          #         Nil
          #       }
          #     }
          #   }
          # } |
          # new x0 in {
          #   @3!(*x0, 3) |
          #   for(_ <- x0){
          #     Nil
          #   }
          # }
          # """.stripMargin('#')
    assertCompiledEqual(s, t)
  }

  "The normalizer" should "translate a synchronous output into a COMM" in {
    val s = "new x in { x!?(1); x!?(2). }".stripMargin
    val t =
      """
          | new x in {
          |   new r in {
          |     x!(*r, 1) |
          |     for(_ <- r){
          |       new s in {
          |         x!(*s, 2) |
          |         for(_ <- s){
          |           Nil
          |         }
          |       }
          |     }
          |   }
          | }""".stripMargin
    assertCompiledEqual(s, t)
  }
}
