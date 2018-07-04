package coop.rchain.roscala

import coop.rchain.roscala.ob._
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.roscala.CtxtRegName._
import coop.rchain.roscala.VmLiteralName._
import coop.rchain.roscala.ob.expr.TupleExpr
import coop.rchain.roscala.ob.MboxOb.LockedMbox
import coop.rchain.roscala.prim.fixnum.fxPlus

class ActorSpec extends FlatSpec with Matchers {

  def defineActorWithoutUnlock =
    new {

      /**
        * (defOprn return-one)
        * (defActor Foo
        *   (method (return-one)
        *     1
        *   )
        * )
        * (define foo (new Foo))
        */
      val returnOneOprn = new Oprn
      val foo           = new Actor
      val foo1          = new Actor
      val foo2          = new Actor

      /**
        * litvec:
        *   0:   {StdMthd}
        *   1:   {Template}
        * codevec:
        *   0:   extend 1
        *   1:   lit 1,rslt
        *   2:   rtn/nxt
        *
        * For some unknown reason the compiler wants to extend `env` (of
        * the `Ctxt` that gets scheduled by the `return-one` method)
        * with the mapping `Symbol(#self) -> foo`.
        */
      val keyMeta = Meta(extensible = false)
      keyMeta.map.update(Symbol("#self"), LexVariable(0, 0, indirect = false))

      val template = new Template(
        keyTuple = Tuple(Symbol("#self")),
        pat = new IdVecPattern(new TupleExpr(Seq(Symbol("#self")))),
        keyMeta = keyMeta
      )

      val returnOneCode = Code(
        litvec = Seq(Niv, template),
        codevec = Seq(
          OpExtend(1),
          OpImmediateLitToReg(literal = `1`, reg = rslt),
          OpRtn(next = true)
        )
      )

      val returnOneMthd = Mthd(returnOneCode)

      /**
        * Create global environment with `foo` in the first position
        * and the `return-one` operation in the second position.
        */
      val addOprn = new Oprn
      Fixnum.fixnumSbo.meta.add(Fixnum.fixnumSbo, addOprn, fxPlus, ctxt = null)(globalEnv)

      val globalEnv = new GlobalEnv()

      globalEnv.addSlot(Symbol("foo"), foo)
      globalEnv.addSlot(Symbol("return-one"), returnOneOprn)
      globalEnv.addSlot(Symbol("+"), addOprn)
      globalEnv.addSlot(Symbol("foo1"), foo1)
      globalEnv.addSlot(Symbol("foo2"), foo2)

      /**
        * Add key-value pair to `foo` actor instance that maps the
        * `return-one` operation to the `return-one` method of `Foo`.
        */
      foo.meta.add(foo, returnOneOprn, returnOneMthd, ctxt = null)(globalEnv)
      foo.mbox = new EmptyMbox

      foo1.meta.add(foo1, returnOneOprn, returnOneMthd, ctxt = null)(globalEnv)
      foo1.mbox = new EmptyMbox

      foo2.meta.add(foo2, returnOneOprn, returnOneMthd, ctxt = null)(globalEnv)
      foo2.mbox = new EmptyMbox
    }

  "Failing to unlock" should "turn an EmptyMbox into a LockedMbox" in {

    /**
      * Sending a messages to an `Actor` that does not unlock itself,
      * should turn the Actor's mailbox into a `LockedMbox`.
      *
      * (defOprn return-one)
      * (defActor Foo
      *   (method (return-one)
      *     1
      *   )
      * )
      * (define foo (new Foo))
      *
      * `mbox` gets locked and `return-one` method gets invoked.
      * (return-one foo)
      *
      */
    val fixture = defineActorWithoutUnlock

    /** Bytecode for `(return-one foo)` */
    val codevec = Seq(
      OpAlloc(1),
      OpXferGlobalToArg(global = 0, arg = 0),
      OpXferGlobalToReg(global = 1, reg = trgt),
      OpXmit(unwind = false, next = true, nargs = 1)
    )

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, Ctxt.empty, LocRslt)

    Vm.run(ctxt, Vm.State(globalEnv = fixture.globalEnv))

    fixture.foo.mbox shouldBe LockedMbox
  }

  "Dispatching a message" should "invoke a method" in {

    /**
      * Send empty message to an `Actor` that in response invokes
      * a `Mthd` that schedules a `Ctxt` that returns `1`.
      *
      * (defOprn return-one)
      * (defActor Foo
      *   (method (return-one)
      *     1
      *   )
      * )
      * (define foo (new Foo))
      *
      * (return-one foo)
      *
      * litvec:
      *   0:   {RequestExpr}
      * codevec:
      *   0:   alloc 1
      *   1:   xfer global[foo],arg[0]
      *   3:   xfer global[return-one],trgt
      *   5:   xmit/nxt 1
      *
      * In `xmit/nxt 1` the VM calls `dispatch` on the `return-one`
      * operation which will call `lookupAndInvoke` on `foo`.
      *
      * `foo.lookupAndInvoke` calls `MboxOb.receive` where the message
      * is received. This also sets the `rcvr` field of the given
      * `Ctxt` to the current `MboxOb`.
      *
      * In `EmptyMbox.receiveMsg` the `mbox` of `foo` is then set to
      * `lockedMbox`. After that the actor processes the task by
      * calling `foo.schedule`
      *
      * `foo.schedule` calls `Ob.lookupAndInvoke` which searches a
      * value for the key `return-one` in `foo` (and in its meta object).
      *
      * The value, which is the `return-one` `Mthd`in this case, then
      * gets invoked. In `Mthd.invoke` the passed in `Ctxt` gets
      * altered with the code of the `return-one` method.
      * The altered `Ctxt` then gets scheduled.
      */
    val fixture = defineActorWithoutUnlock

    /** Bytecode for `(return-one foo)` */
    val codevec = Seq(
      OpAlloc(1),
      OpXferGlobalToArg(global = 0, arg = 0),
      OpXferGlobalToReg(global = 1, reg = trgt),
      OpXmit(unwind = false, next = true, nargs = 1)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, Vm.State(globalEnv = fixture.globalEnv))

    rtnCtxt.rslt shouldBe Fixnum(1)
  }

  "(+ (return-one foo1) (return-one foo2))" should "return 2" in {

    /**
      * The two defined actors:
      *
      * (defActor Foo1
      *   (method (return-one) 1))
      * (defActor Foo2
      *   (method (return-one) 1))
      *
      * Should invoke methods on dispatching message:
      *
      * (defOprn return-one)
      *
      *
      * litvec:
      *   0:   {RequestExpr}
      * codevec:
      *   0:   alloc 2
      *   1:   xfer global[+],trgt
      *   3:   outstanding 18,2
      *   5:   push/alloc 1
      *   6:   xfer global[foo2],arg[0]
      *   8:   xfer global[return-one],trgt
      *  10:   xmit 1,arg[1]
      *  11:   pop
      *  12:   push/alloc 1
      *  13:   xfer global[foo1],arg[0]
      *  15:   xfer global[return-one],trgt
      *  17:   xmit/nxt 1,arg[0]
      *  18:   xmit/nxt 2
      */
    val fixture = defineActorWithoutUnlock

    val returnOne = 1
    val plus      = 2
    val foo1      = 3
    val foo2      = 4

    val codevec = Seq(
      OpAlloc(2),
      OpXferGlobalToReg(plus, trgt),
      OpOutstanding(pc = 12, n = 2),
      OpPushAlloc(1),
      OpXferGlobalToArg(global = foo2, arg = 0),
      OpXferGlobalToReg(global = returnOne, reg = trgt),
      OpXmitArg(unwind = false, next = false, nargs = 1, arg = 1),
      OpPop,
      OpPushAlloc(1),
      OpXferGlobalToArg(foo1, 0),
      OpXferGlobalToReg(returnOne, trgt),
      OpXmitArg(unwind = false, next = true, nargs = 1, arg = 0),
      OpXmit(unwind = false, next = true, nargs = 2)
    )

    val rtnCtxt = Ctxt.outstanding(1)

    val code = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt = Ctxt(code, rtnCtxt, LocRslt)

    Vm.run(ctxt, Vm.State(globalEnv = fixture.globalEnv))

    rtnCtxt.rslt shouldBe Fixnum(2)
  }
}
