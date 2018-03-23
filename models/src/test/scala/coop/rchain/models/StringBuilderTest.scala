package coop.rchain.models

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models._
import coop.rchain.models.Expr.ExprInstance._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import implicits._
import coop.rchain.models._

import scala.collection.immutable.BitSet

class StringBuilderTest extends FlatSpec with PropertyChecks with Matchers {

  val p: Par = Par()

  "New" should "use 0-based indexing" in {
    val neu    = p.copy(news = Seq(New(3, Some(Par()))))
    val result = StringBuilder().buildString(neu)
    val target = "new x0, x1, x2 in { Nil }"
    result shouldBe target
  }

  "Par" should "pretty print" in {
    val source: Par = p.copy(
      exprs = Seq(Expr(exprInstance = GInt(0)),
                  Expr(exprInstance = GBool(true)),
                  Expr(exprInstance = GString("2")),
                  Expr(exprInstance = GUri("www.3cheese.com"))),
      ids = Seq(GPrivate("4"), GPrivate("5"))
    )
    val result = StringBuilder().buildString(source)
    println(result)
    val target = "0 | true | 2 | www.3cheese.com | 4 | 5"
    result shouldBe target
  }

  "Send" should "pretty print" in {
    val source: Par = p.copy(
      sends = Seq(Send(Some(Channel(Quote(Par()))), List[Par](Par(), Par()), true, 0, BitSet())))
    val result = StringBuilder().buildString(source)
    val target = "@{ Nil }!!(Nil, Nil)"
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

  "Reducible" should "use variable names consistently" in {
    val source = p
      .addNews(
        New(
          1,
          Some(
            p.copy(
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
              ),
              sends = Seq(
                Send(Some(Channel(ChanVar(Var(BoundVar(0))))), List[Par](Par()), false, 0, BitSet())
              )
            ))
        )
      )

    val result = StringBuilder().buildString(source)
    val target = "new x0 in { x0!(Nil) | for( x1 <- x0 ) { *x1 } }"
    result shouldBe target
  }

}
