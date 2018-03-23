package coop.rchain.models

import coop.rchain.models.Channel.ChannelInstance._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}

class StringBuilderTest extends FlatSpec with PropertyChecks with Matchers {

  val p: Par = Par()

  "New" should "use 0-based indexing" in {
    val neu    = p.copy(news = Seq(New(3, Some(Par()))))
    val result = StringBuilder().buildString(neu)
    val target = "new x0, x1, x2 in { Nil }"
    result shouldBe target
  }

  "Receive" should "use variable names consistently" in {

    val source = p.copy(
      news = Seq(New(
        1,
        Some(p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(Channel(ChanVar(Var(FreeVar(0))))),
                  Some(Channel(ChanVar(Var(BoundVar(0)))))
                )
              ),
              Some(
                p.copy(evals = Seq(Eval(Some(Channel(ChanVar(Var(BoundVar(0))))))))
              )
            )
          )
        ))
      )))

    val result = StringBuilder().buildString(source)
    val target = "new x0 in { for( x1 <- x0 ) { *x1 } }"
    result shouldBe target
  }

  "Receive" should "use variable names consistently with multiple binds" in {

    val source = p.copy(
      news = Seq(New(
        1,
        Some(p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(Channel(ChanVar(Var(FreeVar(0))))),
                  Some(Channel(ChanVar(Var(BoundVar(0)))))
                ),
                ReceiveBind(
                  Seq(Channel(ChanVar(Var(FreeVar(0))))),
                  Some(Channel(ChanVar(Var(BoundVar(0)))))
                )
              ),
              Some(
                p.copy(evals = Seq(Eval(Some(Channel(ChanVar(Var(BoundVar(1)))))),
                                   Eval(Some(Channel(ChanVar(Var(BoundVar(0))))))))
              )
            )
          )
        ))
      )))

    val result = StringBuilder().buildString(source)
    val target = "new x0 in { for( x1 <- x0 ; x2 <- x0 ) { *x1 | *x2 } }"
    result shouldBe target
  }

}
