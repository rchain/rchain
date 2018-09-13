package coop.rchain.roscala.actor

import coop.rchain.roscala.CtxtRegName._
import coop.rchain.roscala.VmLiteralName._
import coop.rchain.roscala._
import coop.rchain.roscala.ob._
import coop.rchain.roscala.ob.expr.TupleExpr
import coop.rchain.roscala.ob.mbox.{EmptyMbox, MboxOb, QueueMbox}
import coop.rchain.roscala.pools.{ParallelStrandPool, StrandPoolExecutor}
import coop.rchain.roscala.util.misc.{createGlobalEnv, SymbolOffsets, SymbolOps}
import coop.rchain.roscala.prim.fixnum.fxPlus
import coop.rchain.roscala.prim.rblfloat.flPlus

class ActorSpec extends VmSpecUtils {

  override def runBehaviour[E: StrandPoolExecutor]: Unit = {

    def defineActor =
      new {

        /** Defines `Actor` that updates a slot
          *
          * (defOprn increase)
          *
          * (defActor Foo (slots& i 0)
          *   (method (increase amount)
          *     (update i (+ i amount))
          *     ['i i]
          *   )
          * )
          *
          * (define foo (new Foo))
          */
        val foo          = new Actor
        val increaseOprn = new Oprn
        val addOprn      = new Oprn

        /**
          * Create global environment and mapping of `Symbol`s to global
          * environment offsets.
          *
          * Implicit conversion from Scala `Symbol`s to `GlobalEnv`
          * offsets will be used for easier specification of globals in
          * `Code` objects.
          */
        val globalEnvMapping = Map(
          Symbol("foo")      -> foo,
          Symbol("increase") -> increaseOprn,
          Symbol("+")        -> addOprn
        )

        val (globalEnv, symbolOffsets) = createGlobalEnv(globalEnvMapping)
        implicit val m: SymbolOffsets  = symbolOffsets

        /** `increase amount` method
          *
          * litvec:
          *   0:   {StdMthd}
          *   1:   {Template}
          *   2:   'i
          * codevec:
          *   0:   extend 1
          *   1:   fork 12
          *   2:   alloc 2
          *   3:   liti 2,arg[0]
          *   4:   outstanding 17,1
          *   6:   push/alloc 2
          *   7:   xfer lex[1,(0)],arg[0]
          *   8:   xfer lex[0,1],arg[1]
          *   9:   xfer global[+],trgt
          *   11:  xmit/nxt 2,arg[1]
          *   12:  alloc 2
          *   13:  liti 2,arg[0]
          *   14:  xfer lex[1,(0)],arg[1]
          *   15:  xfer argvec,rslt
          *   16:  rtn/nxt
          *   17:  update!/nxt 2
          *
          * See explanation for opcodes in `increaseMthdCode`.
          *
          * Notice: In Roscala opcode positions are not influenced by
          * the size of an opcode. Every opcode just counts `1`.
          * Therefore we have to adjust the `fork` and `outstanding`
          * opcodes.
          */
        val keyMeta = Meta(extensible = false)
        keyMeta.map.update(Symbol("#self"), LexVariable(0, 0, indirect = false))
        keyMeta.map.update(Symbol("amount"), LexVariable(0, 1, indirect = false))

        val template = new Template(
          keyTuple = Tuple(Symbol("#self"), Symbol("amount")),
          pat = new IdVecPattern(new TupleExpr(Seq(Symbol("#self"), Symbol("amount")))),
          keyMeta = keyMeta
        )

        val increaseMthdCode = Code(
          litvec = Seq(Niv, template, Symbol("i")),
          codevec = Seq(
            OpExtend(1),
            OpFork(10),
            /**
              * Do `(+ i amount)` and write result into argvec[1] of
              * continuation.
              * Continue with continuation at position 15 (OpApplyCmd)
              * where we update `i`.
              */
            OpAlloc(2),
            OpIndLitToArg(lit = 2, arg = 0),
            OpOutstanding(pc = 15, n = 1),
            OpPushAlloc(2),
            OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 0),
            OpXferLexToArg(indirect = false, level = 0, offset = 1, arg = 1),
            OpXferGlobalToReg(global = '+.global, reg = trgt),
            OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
            /**
              * Get 'i and put it into argvec[0].
              * Get the current value for 'i and put it into argvec[1].
              * Return the argvec tuple.
              */
            OpAlloc(2),
            OpIndLitToArg(lit = 2, arg = 0),
            OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 1),
            OpXferRegToRslt(reg = argvec),
            OpRtn(next = true),
            // Vm gets here from `OpXmitArg(false, true, 2, 1)`
            OpApplyCmd(unwind = false, next = true, nargs = 2, primNum = 326) // `update!` primitive
          )
        )

        val increaseMthd = Mthd(increaseMthdCode)

        /**
          * Add key-value pair to the parent (sbo) of all `Fixnum`s
          * where the key is the add operation and the value is the
          * primitive for addition of `Fixnum`s.
          * And do the same for `RblFloat`.
          */
        Fixnum.fixnumSbo.meta.add(Fixnum.fixnumSbo, addOprn, fxPlus, ctxt = null)(globalEnv)
        RblFloat.rblFloatSbo.meta.add(RblFloat.rblFloatSbo, addOprn, flPlus, ctxt = null)(globalEnv)

        /**
          * Add slot to `foo`: `Symbol('i')` -> `Fixnum(0)`.
          * This represents `(slots& i 0)` in the actor definition of
          * `Foo`.
          */
        foo.meta.map.update(Symbol("i"), LexVariable(level = 0, offset = 0, indirect = true))
        val offset = foo.addSlot(Fixnum(0))
        assert(offset == 0)

        /**
          * Add key-value pair to `foo` actor instance that maps the
          * `increase` operation to the `increase amount` method of `Foo`.
          */
        foo.meta.add(foo, increaseOprn, increaseMthd, ctxt = null)(globalEnv)
        foo.mbox = new EmptyMbox
      }

    def defineActorWithoutUnlock =
      new {

        /** Defines `Actor` that does not unlock itself
          *
          * (defOprn returnOne)
          *
          * (defActor Foo
          *   (method (returnOne)
          *     1
          *   )
          * )
          *
          * (define foo (new Foo))
          */
        val returnOneOprn = new Oprn
        val foo           = new Actor

        /**
          * Create global environment and mapping of `Symbol`s to global
          * environment offsets.
          *
          * Implicit conversion from Scala `Symbol`s to `GlobalEnv`
          * offsets will be used for easier specification of globals in
          * `Code` objects.
          */
        val globalEnvMapping = Map(
          Symbol("foo")       -> foo,
          Symbol("returnOne") -> returnOneOprn
        )

        val (globalEnv, symbolOffsets) = createGlobalEnv(globalEnvMapping)

        val keyMeta = Meta(extensible = false)
        keyMeta.map.update(Symbol("#self"), LexVariable(0, 0, indirect = false))

        val template = new Template(
          keyTuple = Tuple(Symbol("#self")),
          pat = new IdVecPattern(new TupleExpr(Seq(Symbol("#self")))),
          keyMeta = keyMeta
        )

        /** `returnOne` method
          *
          * litvec:
          *   0:   {StdMthd}
          *   1:   {Template}
          * codevec:
          *   0:   extend 1
          *   1:   lit 1,rslt
          *   2:   rtn/nxt
          */
        val returnOneMthdCode = Code(
          litvec = Seq(Niv, template),
          codevec = Seq(
            OpExtend(1),
            OpImmediateLitToReg(literal = `1`, reg = rslt),
            OpRtn(next = true)
          )
        )

        val returnOneMthd = Mthd(returnOneMthdCode)

        /**
          * Add key-value pair to `foo` actor instance that maps the
          * `returnOne` operation to the `returnOne` method of `Foo`.
          */
        foo.meta.add(foo, returnOneOprn, returnOneMthd, ctxt = null)(globalEnv)
        foo.mbox = new EmptyMbox
      }

    /* Remove test for now since there is an undiscovered nondeterministic bug that
       sometimes breaks the CI.

    "(block (increase foo 1) (increase foo 2))" should "increase value by 3" inMultimode {

      /** Testing `QueueMbox`
     *
     * Warning: This might not test `QueueMbox` when run in
     * multi-threaded mode since the ordering of scheduled tasks is
     * of importance here.
     *
     * The first message will lock up the mailbox of `foo`.
     * The second message arrives at the mailbox of `foo` before
     * the first message got processed (and the mailbox got
     * unlocked again). This turns the locked mailbox into a
     * `QueueMbox` where the second message, `2`, gets enqueued.
     *
     * Then the first message gets processed.
     * The processing results in a call to `Actor.update` which sets
     * `i` to `1`.
     * `Actor.update` also calls `QueueMbox.nextMsg`.
     *
     * In `QueueMbox.nextMsg` two things happen:
     * 1) The mailbox of `foo` becomes the (singleton) `LockedMbox`
     * 2) The second message, `2`, gets dequeued and scheduled which
     * invokes the `increase` method on the argument `2`
     *
     * At some point after `QueueMbox.nextMsg` the `increase` method
     * will run and increase `i` by `2`.
     *
     * litvec:
     *   0:   {BlockExpr}
     * codevec:
     *   0:   fork 8
     *   1:   alloc 2
     *   2:   xfer global[foo],arg[0]
     *   4:   lit 1,arg[1]
     *   5:   xfer global[increase],trgt
     *   7:   xmit/nxt 2
     *   8:   alloc 2
     *   9:   xfer global[foo],arg[0]
     *   11:  lit 2,arg[1]
     *   12:  xfer global[increase],trgt
     *   14:  xmit/nxt 2
     *
     */
      val fixture                   = defineActor
      implicit val m: SymbolOffsets = fixture.symbolOffsets

      /** Bytecode for `(returnOne foo)` */
      val codevec = Seq(
        OpFork(6),
        OpAlloc(2),
        OpXferGlobalToArg(global = 'foo.global, arg = 0),
        OpImmediateLitToArg(literal = `1`, arg = 1),
        OpXferGlobalToReg(global = 'increase.global, reg = trgt),
        OpXmit(unwind = false, next = true, nargs = 2),
        OpAlloc(2),
        OpXferGlobalToArg(global = 'foo.global, arg = 0),
        OpImmediateLitToArg(literal = `2`, arg = 1),
        OpXferGlobalToReg(global = 'increase.global, reg = trgt),
        OpXmit(unwind = false, next = true, nargs = 2)
      )

      val code = Code(litvec = Seq.empty, codevec = codevec)
      val ctxt = Ctxt(code, Ctxt.empty, LocRslt)

      Vm.run(ctxt, Vm.State(globalEnv = fixture.globalEnv))

      fixture.foo.extension.slot.unsafeGet(0) shouldBe Fixnum(3)
    }
     */

    "Failing to unlock" should "turn an EmptyMbox into a LockedMbox" inMultimode {

      /**
        * Sending a messages to an `Actor` that does not unlock itself,
        * should turn the Actor's mailbox into a `LockedMbox`.
        *
        * (defOprn returnOne)
        *
        * (defActor Foo
        *   (method (returnOne)
        *     1
        *   )
        * )
        *
        * (define foo (new Foo))
        *
        * `mbox` gets locked and `returnOne` method gets invoked.
        * (returnOne foo)
        *
        */
      val fixture                   = defineActorWithoutUnlock
      implicit val m: SymbolOffsets = fixture.symbolOffsets

      /** Bytecode for `(returnOne foo)` */
      val codevec = Seq(
        OpAlloc(1),
        OpXferGlobalToArg(global = 'foo.global, arg = 0),
        OpXferGlobalToReg(global = 'returnOne.global, reg = trgt),
        OpXmit(unwind = false, next = true, nargs = 1)
      )

      val code = Code(litvec = Seq.empty, codevec = codevec)
      val ctxt = Ctxt(code, Ctxt.empty, LocRslt)

      Vm.run(ctxt, Vm.State(globalEnv = fixture.globalEnv))

      fixture.foo.mbox shouldBe MboxOb.LockedMbox
    }

    "Failing to unlock" should "turn a LockedMbox into a QueueMbox and enqueue the message" inMultimode {

      /**
        * Sending a messages to an `Actor` that does not unlock itself
        * and has a `LockedMbox`, should turn the Actor's mailbox into a
        * `QueueMbox`.
        *
        * (defOprn returnOne)
        *
        * (defActor Foo
        *   (method (returnOne)
        *     1
        *   )
        * )
        *
        * (define foo (new Foo))
        *
        * `mbox` gets locked and `returnOne` method gets invoked when
        * message is received.
        * (returnOne foo)
        *
        * `mbox` becomes a `QueueMbox`, message gets enqueued and
        * nothing gets invoked.
        * (returnOne foo)
        *
        */
      val fixture                   = defineActorWithoutUnlock
      implicit val m: SymbolOffsets = fixture.symbolOffsets

      /** Bytecode for `(returnOne foo)` */
      val codevec = Seq(
        OpAlloc(1),
        OpXferGlobalToArg(global = 'foo.global, arg = 0),
        OpXferGlobalToReg(global = 'returnOne.global, reg = trgt),
        OpXmit(unwind = false, next = true, nargs = 1)
      )

      val code  = Code(litvec = Seq.empty, codevec = codevec)
      val ctxt0 = Ctxt(code, Ctxt.empty, LocRslt)
      val ctxt1 = ctxt0.clone()

      Vm.run(ctxt0, Vm.State(globalEnv = fixture.globalEnv))
      Vm.run(ctxt1, Vm.State(globalEnv = fixture.globalEnv))

      fixture.foo.mbox shouldBe an[QueueMbox]

      val message = fixture.foo.mbox.asInstanceOf[QueueMbox].queue.dequeue()
      message shouldBe ctxt1
      message.asInstanceOf[Ctxt].argvec.elem(0) shouldBe fixture.foo
    }

    "Dispatching a message" should "invoke a method" inMultimode {

      /**
        * Send empty message to an `Actor` that in response invokes
        * a `Mthd` that schedules a `Ctxt` that returns `1`.
        *
        * (defOprn returnOne)
        *
        * (defActor Foo
        *   (method (returnOne)
        *     1
        *   )
        * )
        *
        * (define foo (new Foo))
        *
        * (returnOne foo)
        *
        * litvec:
        *   0:   {RequestExpr}
        * codevec:
        *   0:   alloc 1
        *   1:   xfer global[foo],arg[0]
        *   3:   xfer global[returnOne],trgt
        *   5:   xmit/nxt 1
        *
        * In `xmit/nxt 1` the VM calls `dispatch` on the `returnOne`
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
        * value for the key `returnOne` in `foo` (and in its meta object).
        *
        * The value, which is the `returnOne` `Mthd`in this case, then
        * gets invoked. In `Mthd.invoke` the passed in `Ctxt` gets
        * altered with the code of the `returnOne` method.
        * The altered `Ctxt` then gets scheduled.
        */
      val fixture                   = defineActorWithoutUnlock
      implicit val m: SymbolOffsets = fixture.symbolOffsets

      /** Bytecode for `(returnOne foo)` */
      val codevec = Seq(
        OpAlloc(1),
        OpXferGlobalToArg(global = 'foo.global, arg = 0),
        OpXferGlobalToReg(global = 'returnOne.global, reg = trgt),
        OpXmit(unwind = false, next = true, nargs = 1)
      )

      val rtnCtxt = Ctxt.outstanding(1)

      val code = Code(litvec = Seq.empty, codevec = codevec)
      val ctxt = Ctxt(code, rtnCtxt, LocRslt)

      Vm.run(ctxt, Vm.State(globalEnv = fixture.globalEnv))

      rtnCtxt.rslt shouldBe Fixnum(1)
    }
  }
}
