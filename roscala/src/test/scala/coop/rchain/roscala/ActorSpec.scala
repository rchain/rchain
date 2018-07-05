package coop.rchain.roscala

import coop.rchain.roscala.ob._
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.roscala.CtxtRegName._
import coop.rchain.roscala.VmLiteralName._
import coop.rchain.roscala.ob.expr.TupleExpr
import coop.rchain.roscala.ob.mbox.{EmptyMbox, MboxOb, QueueMbox}

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
      val globalEnv = new GlobalEnv()
      globalEnv.addSlot(Symbol("foo"), foo)
      globalEnv.addSlot(Symbol("return-one"), returnOneOprn)

      /**
        * Add key-value pair to `foo` actor instance that maps the
        * `return-one` operation to the `return-one` method of `Foo`.
        */
      foo.meta.add(foo, returnOneOprn, returnOneMthd, ctxt = null)(globalEnv)
      foo.mbox = new EmptyMbox
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

    Vm.run(ctxt, fixture.globalEnv, Vm.State())

    fixture.foo.mbox shouldBe MboxOb.LockedMbox
  }

  "Failing to unlock" should "turn a LockedMbox into a QueueMbox and enqueue the message" in {

    /**
      * Sending a messages to an `Actor` that does not unlock itself
      * and has a `LockedMbox`, should turn the Actor's mailbox into a
      * `QueueMbox`.
      *
      * (defOprn return-one)
      * (defActor Foo
      *   (method (return-one)
      *     1
      *   )
      * )
      * (define foo (new Foo))
      *
      * `mbox` gets locked and `return-one` method gets invoked when
      * message is received.
      * (return-one foo)
      *
      * `mbox` becomes a `QueueMbox`, message gets enqueued and
      * nothing gets invoked.
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

    val code  = Code(litvec = Seq.empty, codevec = codevec)
    val ctxt0 = Ctxt(code, Ctxt.empty, LocRslt)
    val ctxt1 = ctxt0.clone()

    Vm.run(ctxt0, fixture.globalEnv, Vm.State())
    Vm.run(ctxt1, fixture.globalEnv, Vm.State())

    fixture.foo.mbox shouldBe an[QueueMbox]

    val message = fixture.foo.mbox.asInstanceOf[QueueMbox].queue.dequeue()
    message shouldBe ctxt1
    message.asInstanceOf[Ctxt].argvec.elem(0) shouldBe fixture.foo
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

    Vm.run(ctxt, fixture.globalEnv, Vm.State())

    rtnCtxt.rslt shouldBe Fixnum(1)
  }
}
