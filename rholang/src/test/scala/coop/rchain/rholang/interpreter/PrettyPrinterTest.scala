package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.implicits.{GPrivate, _}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class PrettyPrinterTest extends FlatSpec with PropertyChecks with Matchers {

  val p: Par = Par()

  "New" should "use 0-based indexing" in {
    val neu    = p.copy(news = Seq(New(3, Par())))
    val result = PrettyPrinter().buildString(neu)
    val target = "new x0, x1, x2 in { Nil }"
    result shouldBe target
  }

  "Par" should "pretty print" in {
    val source: Par = p.copy(
      exprs = Seq(GInt(0), GBool(true), GString("2"), GUri("www.3cheese.com")),
      ids = Seq(GPrivate("4"), GPrivate("5"))
    )
    val result = PrettyPrinter().buildString(source)
    val target = "0 | true | \"2\" | `www.3cheese.com` | 4 | 5"
    result shouldBe target
  }

  "Send" should "pretty print" in {
    val source: Par =
      p.copy(sends = Seq(Send(Quote(Par()), List(Par(), Par()), true, 0, BitSet())))
    val result = PrettyPrinter().buildString(source)
    val target = "@{ Nil }!!(Nil, Nil)"
    result shouldBe target
  }

  "Receive" should "print variable names consistently" in {

    // new x0 in { for( z0 <- x0 ) { *x0 } }
    val source = p.copy(
      news = Seq(
        New(1,
            p.copy(
              receives = Seq(
                Receive(
                  Seq(
                    ReceiveBind(
                      Seq(ChanVar(FreeVar(0))),
                      ChanVar(BoundVar(0))
                    )
                  ),
                  p.copy(evals = Seq(Eval(ChanVar(BoundVar(0)))))
                )
              )
            ))))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0 in { for( z1 <- x0 ) { *x1 } }"
    result shouldBe target
  }

  "Receive" should "pretty print multiple patterns" in {

    // new x0 in { for( z0, z1 <- x0 ) { *x1 | *x0 } }
    val source = p.copy(
      news = Seq(New(
        1,
        p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1))),
                  ChanVar(BoundVar(0))
                )
              ),
              p.copy(evals = Seq(Eval(ChanVar(BoundVar(1))), Eval(ChanVar(BoundVar(0)))))
            )
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0 in { for( z1, z2 <- x0 ) { *x1 | *x2 } }"
    result shouldBe target
  }

  "Receive" should "pretty print multiple binds" in {

    // new x0 in { for( z0 <- x0 ; z0 <- x0 ) { *x1 | *x0 } }
    val source = p.copy(
      news = Seq(New(
        1,
        p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0))),
                  ChanVar(BoundVar(0))
                ),
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0))),
                  ChanVar(BoundVar(0))
                )
              ),
              p.copy(evals = Seq(Eval(ChanVar(BoundVar(1))), Eval(ChanVar(BoundVar(0)))))
            )
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0 in { for( z1 <- x0 ; z2 <- x0 ) { *x1 | *x2 } }"
    result shouldBe target
  }

  "Receive" should "pretty print multiple binds with multiple patterns" in {

    // new x0, x1 in { for( z0, z1 <- x0 ; z0, z1 <- x1 ){ *x3 | *x2 | *x1 | *x0 } }
    val source = p.copy(
      news = Seq(New(
        2,
        p.copy(
          receives = Seq(
            Receive(
              Seq(
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1))),
                  ChanVar(BoundVar(1))
                ),
                ReceiveBind(
                  Seq(ChanVar(FreeVar(0)), ChanVar(FreeVar(1))),
                  ChanVar(BoundVar(0))
                )
              ),
              p.copy(evals = Seq(Eval(ChanVar(BoundVar(3))),
                                 Eval(ChanVar(BoundVar(2))),
                                 Eval(ChanVar(BoundVar(1))),
                                 Eval(ChanVar(BoundVar(0)))))
            )
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0, x1 in { for( z2, z3 <- x0 ; z4, z5 <- x1 ) { *x2 | *x3 | *x4 | *x5 } }"
    result shouldBe target
  }

  "Reducible" should "use variable names consistently" in {
    val source = p.copy(
      news = Seq(New(
        1,
        p.copy(
          receives = Seq(
            Receive(
              Seq(ReceiveBind(
                Seq(ChanVar(FreeVar(0))),
                ChanVar(BoundVar(0))
              )),
              p.copy(evals = Seq(Eval(ChanVar(BoundVar(0)))))
            )
          ),
          sends = Seq(
            Send(ChanVar(BoundVar(0)), List(Par()), false, 0, BitSet())
          )
        )
      )))

    val result = PrettyPrinter().buildString(source)
    val target = "new x0 in { x0!(Nil) | for( z1 <- x0 ) { *x1 } }"
    result shouldBe target
  }
}
