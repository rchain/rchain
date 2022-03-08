package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.ParBuilderUtil.assertCompiledEqual
import org.scalatest.{FlatSpec, Matchers}

class ComplexConsumeSpec extends FlatSpec with Matchers {

  "The normalizer" should "translate consumes with send-receive sources into COMMs" in {

    val s =
      """
        # for(ptrn1, ptrn2, ... @ptrn3 <- @1!?(2, 3, 4, 5)){
        #   for(ptrn4, ptrn5, ... @ptrn6 <- @2?!){ Nil }
        # }""".stripMargin('#')

    val t =
      """
        # new r in {
        #   @1!(*r, 2, 3, 4, 5) |
        #   for(ptrn1, ptrn2, ... @ptrn3 <- r){
        #     for(s, ptrn4, ptrn5, ... @ptrn6 <- @2){
        #       s!() | Nil
        #     }
        #   }
        # }""".stripMargin('#')

    assertCompiledEqual(s, t)
  }

  "The normalizer" should "translate consumes with receive-send sources into COMMs" in {

    val s =
      """
        # for(ptrn1, ptrn2, ... @ptrn3 <- @1!?(2, 3, 4, 5)){
        #   for(ptrn4, ptrn5, ... @ptrn6 <- @2?!){ Nil }
        # }""".stripMargin('#')

    val t =
      """
        # new r in {
        #   @1!(*r, 2, 3, 4, 5) |
        #   for(ptrn1, ptrn2, ... @ptrn3 <- r){
        #     for(s, ptrn4, ptrn5, ... @ptrn6 <- @2){
        #       s!() | Nil
        #     }
        #   }
        # }""".stripMargin('#')

    assertCompiledEqual(s, t)
  }
}
