package coop.rchain.rholang.interpreter

import org.scalatest.{FlatSpec, Matchers}
import Substitute._
import ExprTest._
import scala.collection.immutable.HashMap

object ExprTest {
  val target0: Channel = ChanVar(BoundVar(0))
  val name0: Quote = Par.fromGPrivate
  val target1: Channel = ChanVar(BoundVar(1))
  val name1: Quote = Par.fromGPrivate
  val name2: Quote = Par.fromGPrivate
  val target2: Channel = ChanVar(BoundVar(2))
  val par0: Par = Par()
  val target3: Channel = ChanVar(BoundVar(3))
  val par1: Par = Par(New(2, Par()))
  val par2: Par = Par(New(2, Par(Send(ChanVar(BoundVar(0)), List(Par()), persistent = false))))
}

class ChannelSub extends FlatSpec with Matchers {

  s"$target0" should "be decremented by 0" in {
    val env = HashMap.empty[Int, Data]
    val result = substitute(target0, env).left.get
    result should be(target0)
  }

  s"$target0" should s"be substituted for $name0" in {
    val env = HashMap(0 -> Left(name0))
    val result = substitute(target0, env).left.get
    result should be(name0)
  }


  s"$target1" should "be decremented by 1" in {
    val env = HashMap(0 -> Left(name0))
    val result = substitute(target1, env).left.get
    result should be(name0)
  }


  s"$target2" should s"be decremented by 2" in {
    val env = HashMap(0 -> Left(name0), 1 -> Left(name1))
    val result = substitute(target1, env).left.get
    result should be(name0)
  }


  s"$target0" should s"be substituted for $par0" in {
    val env = HashMap(0 -> Right(par0))
    val result = substitute(target0, env).right.get
    result should be(par0)
  }

  s"$target0" should s"be substituted for $par1 with no visible renamings" in {
    val env = HashMap(0 -> Right(par1))
    val result = substitute(target0, env).right.get
    result should be(par1)
  }

  s"$target0" should s"be substituted for $par2 with 1 renaming" in {
    val env = HashMap(0 -> Right(par2))
    val result = substitute(target0, env).right.get
    result should be(Par(New(2, Par(Send(ChanVar(BoundVar(1)), List(Par()), persistent = false)))))
  }
}

class ParSub extends FlatSpec with Matchers {
  val par3 = Par(
    New(2,
      Par(
        Send(
          ChanVar(BoundVar(0)),
          List(Par()),
          persistent = false
        )
      ).copy(
        receives = List(
          Receive(List((List(ChanVar(FreeVar(0))), ChanVar(BoundVar(0))),
            (List(ChanVar(FreeVar(1))), ChanVar(BoundVar(1)))),
            Par(Eval(ChanVar(BoundVar(2)))),
            persistent = false,
            1))))
  )

  s"In $par3, $target0" should s"be substituted for $name0, $target1 should be substituted for $name1" in {
    val env = HashMap(0 -> Left(name0), 1 -> Left(name1))
    val result = substitute(par3, env)
    result should be(
      Par(
        New(2,
          Par(
            Send(
              name0,
              List(Par()),
              persistent = false
            )
          ).copy(
            receives = List(
              Receive(
                List(
                  (List(ChanVar(FreeVar(0))), name0),
                  (List(ChanVar(FreeVar(1))), name1)
                ),
                Par(Eval(ChanVar(BoundVar(2)))),
                persistent = false,
                1
              )))))
    )
  }

  val par4 =
    Par(
      Send(
        target0,
        List(Par(Eval(target0))),
        persistent = false
      )
    )

  s"In $par4, $target0" should s"be substituted for ${Quote(Par())}" in {
    val env = HashMap(0 -> Left(Quote(Par())))
    val result = substitute(par4,env)
    result should be (
      Par(
        Send(
          Quote(Par()),
          List(Par(Eval(Quote(Par())))),
          persistent = false
        )
      )
    )
  }

  val par5 =
    Par(
      Send(
        ChanVar(BoundVar(0)),
        List(
          Par(
            Receive(
              List((List(ChanVar(FreeVar(0))),ChanVar(BoundVar(0)))),
              Par(Eval(ChanVar(BoundVar(1)))),
              persistent = false,
              1
            )
          )
        ),
        persistent = false
      )
    )

  s"In $par5, $target0" should s"be substituted for $name0" in {
    val env = HashMap(0 -> Left(name0))
    val result = substitute(par5,env)
    result should be(
      Par(
        Send(
          name0,
          List(
            Par(
              Receive(
                List((List(ChanVar(FreeVar(0))),name0)),
                Par(Eval(ChanVar(BoundVar(0)))),
                persistent = false,
                1
              )
            )
          ),
          persistent = false
        )
      )
    )
  }
}
