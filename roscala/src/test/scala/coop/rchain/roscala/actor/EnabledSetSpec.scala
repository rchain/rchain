package coop.rchain.roscala.actor

import coop.rchain.roscala.CtxtRegName._
import coop.rchain.roscala.VmLiteralName._
import coop.rchain.roscala._
import coop.rchain.roscala.ob._
import coop.rchain.roscala.ob.expr.TupleExpr
import coop.rchain.roscala.ob.mbox.{EmptyMbox, QueueMbox}
import coop.rchain.roscala.pools.{SimpleStrandPool, StrandPoolExecutor}
import coop.rchain.roscala.prim.fixnum.{fxEq, fxLt, fxMinus}
import coop.rchain.roscala.prim.ob.objectIndexedSize
import coop.rchain.roscala.util.misc.{createGlobalEnv, SymbolOffsets, SymbolOps}
import coop.rchain.roscala.prim.tuple.{tplConcat, tplHead, tplTail}

class EnabledSetSpec extends VmSpecUtils {

  override def runBehaviour[S](implicit ex: StrandPoolExecutor[S]): Unit = {

    def defineActorEnabledSet = new {

      /** Defines an `Actor` which makes use of enabled sets
        *
        * An enabled set works like a filter on an actor's mailbox.
        *
        * The actor that is defined here will only process `enq`
        * messages as long as the `buf` slot has not reached its limit.
        * At the same time we only allow for `deq` messages while `buf`
        * is not empty.
        *
        * (defActor Fifo (slots& buf [] lim 3)
        *   (local (empty) [[enq]])
        *   (local (full) [[deq]])
        *   (local (partial) [[enq] [deq]])
        *
        *   (method (enq item)
        *     (next
        *       (if (< (size buf) (- lim 1)) (partial) (full))
        *       buf (concat buf [item])
        *     )
        *     ['newbuf (concat buf [item])]
        *   )
        *
        *   (method (deq)
        *     (next
        *       (if (<= (size buf) 1) (empty) (partial))
        *       buf (tail buf)
        *     )
        *     ['dequeued (head buf) 'newbuf (tail buf)]
        *   )
        * )
        */
      val fifo = new Actor

      /**
        * Create global environment and mapping of `Symbol`s to global
        * environment offsets.
        *
        * Implicit conversion from Scala `Symbol`s to `GlobalEnv`
        * offsets will be used for easier specification of globals in
        * `Code` objects.
        */
      val tailOprn   = new Oprn
      val headOprn   = new Oprn
      val sizeOprn   = new Oprn
      val eqOprn     = new Oprn
      val concatOprn = new Oprn
      val ltOprn     = new Oprn
      val minusOprn  = new Oprn
      val enqOprn    = new Oprn
      val deqOprn    = new Oprn

      val globalEnvMapping = Map(
        Symbol("tail")   -> tailOprn,
        Symbol("head")   -> headOprn,
        Symbol("size")   -> sizeOprn,
        Symbol("=")      -> eqOprn,
        Symbol("concat") -> concatOprn,
        Symbol("<")      -> ltOprn,
        Symbol("enq")    -> enqOprn,
        Symbol("deq")    -> deqOprn,
        Symbol("fifo")   -> fifo,
        Symbol("-")      -> minusOprn
      )

      val (globalEnv, symbolsOffset) = createGlobalEnv(globalEnvMapping)
      implicit val m: SymbolOffsets  = symbolsOffset

      /** `empty` method
        *
        * litvec:
        *   0:   {StdMthd}
        *   1:   {Template}
        * codevec:
        *   0:   extend 1
        *   1:   alloc 1
        *   2:   outstanding 9,1
        *   4:   push/alloc 1
        *   5:   xfer global[enq],arg[0]
        *   7:   xfer argvec,rslt
        *   8:   rtn/nxt arg[0]
        *   9:   xfer argvec,rslt
        *   10:  rtn/nxt
        */
      val keyMetaEmpty = Meta(extensible = false)
      keyMetaEmpty.map.update(Symbol("#self"), LexVariable(0, 0, indirect = false))

      val templateEmpty = new Template(
        keyTuple = Tuple(Symbol("#self")),
        pat = new IdVecPattern(new TupleExpr(Seq(Symbol("#self")))),
        keyMeta = keyMetaEmpty
      )

      val emptyMthdCode = Code(
        litvec = Seq(Niv, templateEmpty),
        codevec = Seq(
          OpExtend(lit = 1),
          OpAlloc(n = 1),
          OpOutstanding(pc = 7, n = 1),
          OpPushAlloc(n = 1),
          OpXferGlobalToArg(global = 'enq.global, arg = 0),
          OpXferRegToRslt(reg = argvec),
          OpRtnArg(next = true, arg = 0),
          OpXferRegToRslt(reg = argvec),
          OpRtn(next = true)
        )
      )

      val emptyMthd = Mthd(emptyMthdCode)

      /** `full` method
        *
        * litvec:
        *   0:   {StdMthd}
        *   1:   {Template}
        * codevec:
        *   0:   extend 1
        *   1:   alloc 1
        *   2:   outstanding 9,1
        *   4:   push/alloc 1
        *   5:   xfer global[deq],arg[0]
        *   7:   xfer argvec,rslt
        *   8:   rtn/nxt arg[0]
        *   9:   xfer argvec,rslt
        *   10:  rtn/nxt
        */
      val keyMetaFull = Meta(extensible = false)
      keyMetaFull.map.update(Symbol("#self"), LexVariable(0, 0, indirect = false))

      val templateFull = new Template(
        keyTuple = Tuple(Symbol("#self")),
        pat = new IdVecPattern(new TupleExpr(Seq(Symbol("#self")))),
        keyMeta = keyMetaFull
      )

      val fullMthdCode = Code(
        litvec = Seq(Niv, templateFull),
        codevec = Seq(
          OpExtend(lit = 1),
          OpAlloc(n = 1),
          OpOutstanding(pc = 7, n = 1),
          OpPushAlloc(n = 1),
          OpXferGlobalToArg(global = 'deq.global, arg = 0),
          OpXferRegToRslt(reg = argvec),
          OpRtnArg(next = true, arg = 0),
          OpXferRegToRslt(reg = argvec),
          OpRtn(next = true)
        )
      )

      val fullMthd = Mthd(fullMthdCode)

      /** `partial` method
        *
        * litvec:
        *   0:   {StdMthd}
        *   1:   {Template}
        * codevec:
        *   0:   extend 1
        *   1:   alloc 2
        *   2:   outstanding 15,2
        *   4:   push/alloc 1
        *   5:   xfer global[deq],arg[0]
        *   7:   xfer argvec,rslt
        *   8:   rtn arg[1]
        *   9:   pop
        *   10:  push/alloc 1
        *   11:  xfer global[enq],arg[0]
        *   13:  xfer argvec,rslt
        *   14:  rtn/nxt arg[0]
        *   15:  xfer argvec,rslt
        *   16:  rtn/nxt
        */
      val keyMetaPartial = Meta(extensible = false)
      keyMetaPartial.map.update(Symbol("#self"), LexVariable(0, 0, indirect = false))

      val templatePartial = new Template(
        keyTuple = Tuple(Symbol("#self")),
        pat = new IdVecPattern(new TupleExpr(Seq(Symbol("#self")))),
        keyMeta = keyMetaPartial
      )

      val partialMthdCode = Code(
        litvec = Seq(Niv, templatePartial),
        codevec = Seq(
          OpExtend(lit = 1),
          OpAlloc(n = 2),
          OpOutstanding(pc = 12, n = 2),
          OpPushAlloc(n = 1),
          OpXferGlobalToArg(global = 'deq.global, arg = 0),
          OpXferRegToRslt(reg = argvec),
          OpRtnArg(next = false, arg = 1),
          OpPop,
          OpPushAlloc(n = 1),
          OpXferGlobalToArg(global = 'enq.global, arg = 0),
          OpXferRegToRslt(reg = argvec),
          OpRtnArg(next = true, arg = 0),
          OpXferRegToRslt(reg = argvec),
          OpRtn(next = true)
        )
      )

      val partialMthd = Mthd(partialMthdCode)

      /** `enq` method
        *
        * litvec:
        *   0:   {StdMthd}
        *   1:   {Template}
        *   2:   'buf
        *   3:   'newbuf
        *   4:   'partial
        *   5:   'full
        * codevec:
        *   0:   extend 1
        *   1:   fork 38 => 30
        *
        *        The call to `next!` needs three arguments:
        *        1) The key to the value that we want to update
        *        2) The new value
        *        3) The enabled set
        *
        *   2:   alloc 3
        *
        *        1) Load key into `arg[1]`.
        *   3:   liti 2,arg[1]
        *
        *        Once we are done with 2) and 3), xmit `next!`.
        *   4:   outstanding 59,2 => 48
        *
        *        2) and 3) are missing at this point.
        *   6:   push/alloc 2
        *          Load `buf` into `arg[0]`
        *   7:     xfer lex[1,(0)],arg[0]
        *   8:     xfer global[concat],trgt
        *
        *          Once done with the following, xmit `concat` and put
        *          result into `arg[2]` of outer ctxt.
        *   10:    outstanding 52,1 => 41
        *
        *          `ctxt.argvec` will be a tuple with one element.
        *   12:    push/alloc 1
        *
        *             Get `item` value.
        *   13:       xfer lex[0,1],arg[0]
        *   14:       xfer argvec,rslt
        *
        *             Make `item` a tuple and return it into `arg[1]`
        *             of parent ctxt.
        *             At this point (in the parent ctxt):
        *               `trgt` = `concat`,
        *               `arg[0]` = `buf`,
        *               `arg[1]` = `[item]`
        *             `rtn` will schedule the parent ctxt which will
        *             continues at line 52.
        *             (At line 52 we xmit `concat` which puts the new
        *             `buf` value into `arg[2]` of the initial ctxt).
        *             We scheduled the work necessary for 2)
        *   15:       rtn arg[1]
        *   16:       pop
        *   17:     pop
        *
        *           At this point:
        *             `arg[1]` = 'buf
        *             `arg[2]` = new `buf` value (once the scheduled ctxt has
        *             actually computed the new `buf` value).
        *
        *         The enabled set is still missing and will go into `arg[0]`.
        *
        *         Throws away `ctxt.pc`, `ctxt.outstanding`,
        *         `ctxt.argvec` but keeps things like `ctxt.code` and
        *         also keeps the initial ctxt as its continuation.
        *   18:   push
        *
        *           Once we are done with the following, either return
        *           `(partial)` or `(full)`.
        *   19:     outstanding 54,1 => 43
        *   21:     push/alloc 2
        *   22:       xfer global[<],trgt
        *   24:       outstanding 53,2 => 42
        *   26:       push/alloc 2
        *   27:         xfer lex[1,(1)],arg[0]
        *   28:         lit 1,arg[1]
        *   29:         xfer global[-],trgt
        *   31:         xmit 2,arg[1]
        *   32:         pop
        *   33:       push/alloc 1
        *   34:         xfer lex[1,(0)],arg[0]
        *   35:         xfer global[size],trgt
        *   37:         xmit/nxt 1,arg[0]
        *
        *   Create the result tuple and put it into the `rslt`
        *   register. Then return it at line 63.
        *
        *   38:   alloc 2
        *   39:   liti 3,arg[0]
        *   40:   outstanding 63,1 => 50
        *   42:   push/alloc 2
        *
        *           Get `item`
        *   43:     xfer lex[1,(0)],arg[0]
        *   44:     xfer global[concat],trgt
        *   46:     outstanding 62,1 => 49
        *   48:     push/alloc 1
        *   49:       xfer lex[0,1],arg[0]
        *   50:       xfer argvec,rslt
        *   51:       rtn/nxt arg[1]
        *
        *   52:   xmit/nxt 2,arg[2]
        *   53:   xmit/nxt 2,rslt
        *
        *         If `rslt` == `RblFalse` (result of if-statement), go to
        *         line 57.
        *   54:   jf 57 => 46
        *
        *         Lookup the value for `'partial` which is a method.
        *   55:   lookup 4,trgt
        *
        *         Return the result of running `(partial)` to `arg[0]` in the
        *         initial ctxt (this should schedule the initial ctxt).
        *   56:   xmit/nxt 0,arg[0]
        *   57:   lookup 5,trgt
        *   58:   xmit/nxt 0,arg[0]
        *   59:   next!/nxt 3
        *   62:   xmit/nxt 2,arg[1]
        *   63:   xfer argvec,rslt
        *   64:   rtn/nxt
        */
      val keyMetaEnq = Meta(extensible = false)
      keyMetaEnq.map.update(Symbol("#self"), LexVariable(0, 0, indirect = false))
      keyMetaEnq.map.update(Symbol("item"), LexVariable(0, 1, indirect = false))

      val templateEnq = new Template(
        keyTuple = Tuple(Symbol("#self"), Symbol("item")),
        pat = new IdVecPattern(new TupleExpr(Seq(Symbol("#self"), Symbol("item")))),
        keyMeta = keyMetaEnq
      )

      val enqMthdCode = Code(
        litvec = Seq(
          Niv,
          templateEnq,
          Symbol("buf"),
          Symbol("newbuf"),
          Symbol("partial"),
          Symbol("full")
        ),
        codevec = Seq(
          OpExtend(lit = 1),
          OpFork(pc = 30),
          /**
            * The call to `next!` needs three arguments:
            *   1) The key to the value that we want to update
            *   2) The new value
            *   3) The enabled set
            */
          OpAlloc(n = 3),
          // 1) Load key into `arg[1]`
          OpIndLitToArg(lit = 2, arg = 1),
          // Once we are done with 2) and 3), xmit `next!`
          OpOutstanding(pc = 48, n = 2),
          // 2) and 3) are missing at this point
          OpPushAlloc(n = 2),
          // Load `buf` into `arg[0]`
          OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 0),
          OpXferGlobalToReg(global = 'concat.global, reg = trgt),
          /**
            * Once done with the following, xmit `concat` and put
            * result into `arg[2]` of outer ctxt
            */
          OpOutstanding(pc = 41, n = 1),
          // `ctxt.argvec` will be a tuple with one element
          OpPushAlloc(n = 1),
          // Get `item` value
          OpXferLexToArg(indirect = false, level = 0, offset = 1, arg = 0),
          OpXferRegToRslt(reg = argvec),
          /**
            * Make `item` a tuple and return it into `arg[1]` of parent
            * ctxt.
            * At this point (in the parent ctxt):
            *   `trgt` = `concat`,
            *   `arg[0]` = `buf`,
            *   `arg[1]` = `[item]`
            * `rtn` will schedule the parent ctxt which will continues
            * at line 52 (at line 52 we xmit `concat` which puts the
            * new `buf` value into `arg[2]` of the initial ctxt).
            *
            * We scheduled the work necessary for 2)
            */
          OpRtnArg(next = false, arg = 1),
          OpPop,
          OpPop,
          /**
            * At this point:
            *   `arg[1]` = 'buf
            *   `arg[2]` = new `buf` value (once the scheduled ctxt has
            * actually computed the new `buf` value)
            *
            * The enabled set is still missing and will go into `arg[0]`.
            */

          /**
            * Throws away `ctxt.pc`, `ctxt.outstanding`, `ctxt.argvec`
            * but keeps things like `ctxt.code` and also keeps the
            * initial ctxt as its continuation.
            */
          OpPush,
          /**
            * Once we are done with the following, either return
            * `(partial)` or `(full)`.
            */
          OpOutstanding(pc = 43, n = 1),
          OpPushAlloc(n = 2),
          OpXferGlobalToReg(global = '<.global, reg = trgt),
          OpOutstanding(pc = 42, n = 2),
          OpPushAlloc(n = 2),
          OpXferLexToArg(indirect = true, level = 1, offset = 1, arg = 0),
          OpImmediateLitToArg(literal = `1`, arg = 1),
          OpXferGlobalToReg(global = '-.global, reg = trgt),
          OpXmitArg(unwind = false, next = false, nargs = 2, arg = 1),
          OpPop,
          OpPushAlloc(n = 1),
          OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 0),
          OpXferGlobalToReg(global = 'size.global, reg = trgt),
          OpXmitArg(unwind = false, next = true, nargs = 1, arg = 0),
          /**
            * Create the result tuple and put it into the `rslt`
            * register.
            * Then return it at `OpXferRegToRslt(reg = argvec)` in the
            * second last line.
            */
          OpAlloc(n = 2),
          OpIndLitToArg(lit = 3, arg = 0),
          OpOutstanding(pc = 50, n = 1),
          OpPushAlloc(n = 2),
          // Get `item`
          OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 0),
          OpXferGlobalToReg(global = 'concat.global, reg = trgt),
          OpOutstanding(pc = 49, n = 1),
          OpPushAlloc(n = 1),
          OpXferLexToArg(indirect = false, level = 0, offset = 1, arg = 0),
          OpXferRegToRslt(reg = argvec),
          OpRtnArg(next = true, arg = 1),
          OpXmitArg(unwind = false, next = true, nargs = 2, arg = 2),
          OpXmitReg(unwind = false, next = true, nargs = 2, reg = rslt),
          // If `rslt` == `RblFalse` (result of if-statement), go to line 57
          OpJmpFalse(pc = 46),
          // Lookup the value for `'partial` which is a method
          OpLookupToReg(lit = 4, reg = trgt),
          /**
            * Return the result of running `(partial)` to `arg[0]` in
            * the initial ctxt (this should schedule the initial ctxt).
            */
          OpXmitArg(unwind = false, next = true, nargs = 0, arg = 0),
          OpLookupToReg(lit = 5, reg = trgt),
          OpXmitArg(unwind = false, next = true, nargs = 0, arg = 0),
          OpApplyCmd(unwind = false, next = true, nargs = 3, primNum = 324),
          OpXmitArg(unwind = false, next = true, nargs = 2, arg = 1),
          OpXferRegToRslt(reg = argvec),
          OpRtn(next = true)
        )
      )

      val enqMthd = Mthd(enqMthdCode)

      /** `deq` method
        *
        * litvec:
        *   0:   {StdMthd}
        *   1:   {Template}
        *   2:   'buf
        *   3:   'dequeued
        *   4:   'newbuf
        *   5:   'empty
        *   6:   'partial
        * codevec:
        *   0:   extend 1
        *   1:   fork 26
        *   2:   alloc 3
        *   3:   liti 2,arg[1]
        *   4:   outstanding 48,2
        *   6:   push/alloc 1
        *   7:   xfer lex[1,(0)],arg[0]
        *   8:   xfer global[tail],trgt
        *   10:  xmit 1,arg[2]
        *   11:  pop
        *   12:  push
        *   13:  outstanding 43,1
        *   15:  push/alloc 2
        *   16:  lit 1,arg[1]
        *   17:  xfer global[=],trgt
        *   19:  outstanding 42,1
        *   21:  push/alloc 1
        *   22:  xfer lex[1,(0)],arg[0]
        *   23:  xfer global[size],trgt
        *   25:  xmit/nxt 1,arg[0]
        *   26:  alloc 4
        *   27:  liti 3,arg[0]
        *   28:  liti 4,arg[2]
        *   29:  outstanding 51,2
        *   31:  push/alloc 1
        *   32:  xfer lex[1,(0)],arg[0]
        *   33:  xfer global[tail],trgt
        *   35:  xmit 1,arg[3]
        *   36:  pop
        *   37:  push/alloc 1
        *   38:  xfer lex[1,(0)],arg[0]
        *   39:  xfer global[head],trgt
        *   41:  xmit/nxt 1,arg[1]
        *   42:  xmit/nxt 2,rslt
        *   43:  jf 46
        *   44:  lookup 5,trgt
        *   45:  xmit/nxt 0,arg[0]
        *   46:  lookup 6,trgt
        *   47:  xmit/nxt 0,arg[0]
        *   48:  next!/nxt 3
        *   51:  xfer argvec,rslt
        *   52:  rtn/nxt
        */
      val keyMetaDeq = Meta(extensible = false)
      keyMetaDeq.map.update(Symbol("#self"), LexVariable(0, 0, indirect = false))
      keyMetaDeq.map.update(Symbol("item"), LexVariable(0, 1, indirect = false))

      val templateDeq = new Template(
        keyTuple = Tuple(Symbol("#self"), Symbol("item")),
        pat = new IdVecPattern(new TupleExpr(Seq(Symbol("#self"), Symbol("item")))),
        keyMeta = keyMetaDeq
      )

      val deqMthdCode = Code(
        litvec = Seq(
          Niv,
          templateDeq,
          Symbol("buf"),
          Symbol("dequeued"),
          Symbol("newbuf"),
          Symbol("empty"),
          Symbol("partial")
        ),
        codevec = Seq(
          OpExtend(lit = 1),
          OpFork(pc = 20),
          OpAlloc(n = 3),
          OpIndLitToArg(lit = 2, arg = 1),
          OpOutstanding(pc = 39, n = 2),
          OpAlloc(n = 1),
          OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 0),
          OpXferGlobalToReg(global = 'head.global, reg = trgt),
          OpXmitArg(unwind = false, next = false, nargs = 1, arg = 2),
          OpPop,
          OpPush,
          OpOutstanding(pc = 34, n = 1),
          OpAlloc(n = 2),
          OpImmediateLitToArg(literal = `1`, arg = 1),
          OpXferGlobalToReg(global = '=.global, reg = trgt),
          OpOutstanding(pc = 33, n = 1),
          OpAlloc(n = 1),
          OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 0),
          OpXferGlobalToReg(global = 'size.global, reg = trgt),
          OpXmitArg(unwind = false, next = true, nargs = 1, arg = 0),
          OpAlloc(n = 4),
          OpIndLitToArg(lit = 3, arg = 0),
          OpIndLitToArg(lit = 4, arg = 2),
          OpOutstanding(pc = 51, n = 2),
          OpAlloc(n = 1),
          OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 0),
          OpXferGlobalToReg(global = 'tail.global, reg = trgt),
          OpXmitArg(unwind = false, next = false, nargs = 1, arg = 3),
          OpPop,
          OpAlloc(n = 1),
          OpXferLexToArg(indirect = true, level = 1, offset = 0, arg = 0),
          OpXferGlobalToReg(global = 'head.global, reg = trgt),
          OpXmitArg(unwind = false, next = true, nargs = 1, arg = 1),
          OpXmitReg(unwind = false, next = true, nargs = 2, reg = trgt),
          OpJmpFalse(pc = 46),
          OpLookupToReg(lit = `5`, reg = trgt),
          OpXmitArg(unwind = false, next = true, nargs = 0, arg = 0),
          OpLookupToReg(lit = `6`, reg = trgt),
          OpXmitArg(unwind = false, next = true, nargs = 0, arg = 0),
          OpApplyCmd(unwind = false, next = true, nargs = 3, primNum = 324), // `next!` primitive
          OpXferRegToRslt(reg = argvec),
          OpRtn(next = true)
        )
      )

      val deqMthd = Mthd(deqMthdCode)

      /**
        * Add key-value pair to the parent (sbo) of all `Tuple`s
        * where the key is the `tail` operation and the value is the
        * `tuple-tail` primitive.
        * Same for `head`, `size` and `=` (for Fixnum)
        */
      Tuple.tupleSbo.meta.add(Tuple.tupleSbo, tailOprn, tplTail, ctxt = null)(globalEnv)
      Tuple.tupleSbo.meta.add(Tuple.tupleSbo, headOprn, tplHead, ctxt = null)(globalEnv)
      Tuple.tupleSbo.meta.add(Tuple.tupleSbo, sizeOprn, objectIndexedSize, ctxt = null)(globalEnv)
      Tuple.tupleSbo.meta.add(Tuple.tupleSbo, concatOprn, tplConcat, ctxt = null)(globalEnv)
      Fixnum.fixnumSbo.meta.add(Fixnum.fixnumSbo, eqOprn, fxEq, ctxt = null)(globalEnv)
      Fixnum.fixnumSbo.meta.add(Fixnum.fixnumSbo, ltOprn, fxLt, ctxt = null)(globalEnv)
      Fixnum.fixnumSbo.meta.add(Fixnum.fixnumSbo, minusOprn, fxMinus, ctxt = null)(globalEnv)

      /**
        * `(slots& buf [] lim 3)`.
        * Adding slots to `fifo` instance.
        */
      fifo.meta.map.update(Symbol("buf"), LexVariable(level = 0, offset = 0, indirect = true))
      assert(fifo.addSlot(Tuple()) == 0)
      fifo.meta.map.update(Symbol("lim"), LexVariable(level = 0, offset = 1, indirect = true))
      assert(fifo.addSlot(Fixnum(3)) == 1)

      /**
        * Adding methods to the actor instance - this would normally go
        * into the SBO/parent of the actor instance.
        */
      fifo.meta.add(fifo, enqOprn, enqMthd, ctxt = null)(globalEnv)
      fifo.meta.add(fifo, deqOprn, deqMthd, ctxt = null)(globalEnv)
      fifo.meta.add(fifo, Symbol("empty"), emptyMthd, ctxt = null)(globalEnv)
      fifo.meta.add(fifo, Symbol("full"), fullMthd, ctxt = null)(globalEnv)
      fifo.meta.add(fifo, Symbol("partial"), partialMthd, ctxt = null)(globalEnv)
      fifo.mbox = new EmptyMbox
    }

    "Removing a method from the enabled set" should
      "result in not processing corresponding messages anymore" inMultimode {

      /** (seq (enq fifo 1) (enq fifo 2) (enq fifo 3) (enq fifo 4))
        *
        * Sending the `enq` message four times, should only process the
        * message for the first three times.
        * When processing the third message, the actor sets the enabled
        * set to the output of `(full)`.
        * This will only allow for the processing of `deq` messages in
        * the next state.
        */
      val fixture                   = defineActorEnabledSet
      implicit val m: SymbolOffsets = fixture.symbolsOffset

      /** Bytecode for `(seq (enq fifo 1) (enq fifo 2) (enq fifo 3) (enq fifo 4))` */
      val codevec = Seq(
        OpAlloc(n = 2),
        OpOutstanding(pc = 7, n = 1),
        OpPushAlloc(n = 2),
        OpXferGlobalToArg(global = 'fifo.global, arg = 0),
        OpImmediateLitToArg(literal = `1`, arg = 1),
        OpXferGlobalToReg(global = 'enq.global, reg = trgt),
        OpXmitReg(unwind = false, next = true, nargs = 2, reg = rslt),
        OpOutstanding(pc = 13, n = 1),
        OpPushAlloc(n = 2),
        OpXferGlobalToArg(global = 'fifo.global, arg = 0),
        OpImmediateLitToArg(literal = `2`, arg = 1),
        OpXferGlobalToReg(global = 'enq.global, reg = trgt),
        OpXmitReg(unwind = false, next = true, nargs = 2, reg = rslt),
        OpOutstanding(pc = 19, n = 1),
        OpPushAlloc(n = 2),
        OpXferGlobalToArg(global = 'fifo.global, arg = 0),
        OpImmediateLitToArg(literal = `3`, arg = 1),
        OpXferGlobalToReg(global = 'enq.global, reg = trgt),
        OpXmitReg(unwind = false, next = true, nargs = 2, reg = rslt),
        OpXferGlobalToArg(global = 'fifo.global, arg = 0),
        OpImmediateLitToArg(literal = `4`, arg = 1),
        OpXferGlobalToReg(global = 'enq.global, reg = trgt),
        OpXmit(unwind = false, next = true, nargs = 2)
      )

      val code = Code(litvec = Seq.empty, codevec = codevec)
      val ctxt = Ctxt(code, Ctxt.empty, LocRslt)

      Vm.run(ctxt, Vm.State(globalEnv = fixture.globalEnv))

      /**
        * After the third message, the enabled set is changed to
        * `[[deq]]`.
        */
      fixture.fifo.mbox shouldBe an[QueueMbox]

      /**
        * The `buf` slot of the actor instance should at this point
        * be `[1 2 3]`.
        */
      fixture.fifo.meta.map(Symbol("buf")).get shouldBe LexVariable(
        level = 0,
        offset = 0,
        indirect = true
      )
      fixture.fifo.extension.slot.unsafeGet(0).asInstanceOf[Tuple].numberOfElements() shouldBe 3
      fixture.fifo.extension.slot.unsafeGet(0).asInstanceOf[Tuple].value shouldBe Array(
        Fixnum(1),
        Fixnum(2),
        Fixnum(3)
      )
    }
  }
}
