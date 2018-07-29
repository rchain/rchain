/* Mode: -*- C++ -*- */
// vim: set ai ts=4 sw=4 expandtab
/* @BC
 *		                Copyright (c) 1993
 *	    by Microelectronics and Computer Technology Corporation (MCC)
 *				All Rights Reserved
 *
 *	Permission to use, copy, modify, and distribute this software and its
 *	documentation for any purpose and without fee is hereby granted,
 *	provided that this notice be retained unaltered, and that the name of
 *	MCC and its shareholders and participants shall not be used in
 *	advertising or publicity pertaining to distribution of the software
 *	without specific written prior permission.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "Vm.h"
#include "Code.h"
#include "Compile.h"
#include "CommandLine.h"
#include "Ctxt.h"
#include "Expr.h"
#include "Interrupt.h"
#include "Meta.h"
#include "Monitor.h"
#include "Ob.h"
#include "ObQue.h"
#include "ObStk.h"
#include "Operation.h"
#include "Pattern.h"
#include "Prim.h"
#include "RblStack.h"
#include "Table.h"
#include "Timer.h"
#include "Export.h"

#include "ModuleInit.h"

#include <assert.h>
#include <errno.h>

#ifndef c_plusplus
extern "C" {
int select(int, fd_set*, fd_set*, fd_set*, struct timeval*);
int sigblock(int);
//    int sigpause (int);
int sigsetmask(int);
}
#endif


pOb printer;
Code* printResult;
Code* nxtCode;
Code* rtnNxtCode;


int debugging_level = 0;


VirtualMachine* vm;


VirtualMachine::VirtualMachine() {
    code = (Code*)INVALID;
    ctxt = (pCtxt)INVALID;
    upcallCtxt = (pCtxt)INVALID;
    systemMonitor = (Monitor*)INVALID;
    currentMonitor = (Monitor*)INVALID;
    sleeperPool = new ObStk;
    strandPool = new ObQue;

    resetSignals();

    extAppCBRegistry = (RblTable*)INVALID;
    extAppCBKeyGen = EXTERNAL_APP_CALLBACK_KEY_GEN;

    /*
     * Don't do any allocation that might cause a garbage collection
     * until we have added this virtual machine to the heap's root set
     * collection, and don't add this virtual machine to the heap's root
     * set collection before setting all of the important registers to
     * values that won't wig out the garbage collector.
     */

    heap->addRootSet(this);

    systemMonitor = Monitor::create(SYMBOL("system monitor"));
    currentMonitor = systemMonitor;

    extAppCBRegistry = RblTable::create();
}


VirtualMachine::~VirtualMachine() {
    heap->deleteRootSet(this);
    delete sleeperPool;
    delete strandPool;
}


void VirtualMachine::installEnv(pOb newenv) { ASSIGN(ctxt, env, newenv); }


void VirtualMachine::installCode(Code* cd, int relativePc) {
    code = cd;
    pc.relative = relativePc;
    pc.absolute = code->absolutize(relativePc);
}


void VirtualMachine::installCtxt(pCtxt new_ctxt) {
    if (debugging_level) {
        printf("*** new strand\n");
    }

    ctxt = new_ctxt;
    installCode(ctxt->code, ctxt->pc);
}


void VirtualMachine::installMonitor(Monitor* monitor) {
    if (debugging_level) {
        printf("*** new monitor: %s\n", BASE(monitor->id)->asCstring());
    }

    currentMonitor->stop();
    currentMonitor = monitor;
    currentMonitor->start();
    bytecodes = &currentMonitor->opcodeCounts->word(0);
    Base::obCounts = &currentMonitor->obCounts->word(0);
    debugging_level = currentMonitor->tracing == RBLTRUE;
}


bool VirtualMachine::getNextStrand() {
    while (strandPool->empty()) {
        /*
         * If we get here, we have exhausted all known sources of work,
         * and we want to awaken any sleepers.  If there are none, there
         * really is no more work to do.  If we are are not recognizing
         * async events (i.e, nsigs is zero), we return TRUE to convince
         * the virtual machine to shut down.  If we are recognizing
         * external events we enter sigpause to wait for a prod from some
         * external event.
         */

        if (sleeperPool->empty()) {
            if (nsigs == 0) {
                return true;
            } else {
                if (sigvec == 0) {
                    if (debugging_level) {
                        printf("*** entering sigpause...\n");
                    }

                    sigpause(0);
                    if (debugging_level) {
                        printf("*** exiting sigpause...\n");
                    }
                }

                handleSignal();
                /*
                 * HandleSignal will presumably schedule one or more
                 * strands to be executed.  If it doesn't, we will come
                 * through this body again and try another time.
                 */
            }
        } else {
            if (debugging_level) {
                printf("*** waking sleepers\n");
            }

            while (!sleeperPool->empty()) {
                pCtxt sleeper = (pCtxt)sleeperPool->pop();
                sleeper->scheduleStrand();
            }
        }
    }

    pCtxt strand = (pCtxt)strandPool->deq();
    if (strand->monitor != currentMonitor) {
        installMonitor(strand->monitor);
    }

    installCtxt(strand);
    return false;
}


void VirtualMachine::reset() {
    wrapup();

    sigvec = 0;
    interruptPending = 0;

    RequestExpr* rx = RequestExpr::create(SYMBOL("reset"), NILexpr);
    Code* cp = rx->compileWrt(TopEnv, NIV);
    setup(cp, printResult, ArgReg(0));
}


void VirtualMachine::setup(Code* exprCode, Code* printerCode, Location tag) {
    extern pCtxt makePrinterCtxt(Code*);
    PROTECT(exprCode);
    pCtxt tmp_ctxt = makePrinterCtxt(printerCode);
    tmp_ctxt = Ctxt::create(exprCode, NIL, tmp_ctxt);
    tmp_ctxt->tag = tag;

    const int n = Base::nClasses;
    for (int i = 0; i < n; i++) {
        Base::obCounts[i] = 0;
    }

    heap->resetCounts();

    systemMonitor->reset();

    installCtxt(tmp_ctxt);
    installMonitor(tmp_ctxt->monitor);
}


void VirtualMachine::wrapup() {
    sleeperPool->reset();
    strandPool->reset();

    pc.relative = 0;
    code = (Code*)INVALID;
    ctxt = (pCtxt)INVALID;
    upcallCtxt = (pCtxt)INVALID;

    currentMonitor = systemMonitor;
}


int VirtualMachine::traversePtrs(PSOb__PSOb f) {
    int sum = 0;

    sum += useIfPtr(&code, f);
    sum += useIfPtr(&ctxt, f);
    sum += useIfPtr(&upcallCtxt, f);
    sum += useIfPtr(&systemMonitor, f);
    sum += useIfPtr(&currentMonitor, f);
    sum += sleeperPool->traversePtrs(f);
    sum += strandPool->traversePtrs(f);
    int i = 0;
    for (; i < NSIG; i++) {
        sum += useIfPtr(&sigPool[i], f);
    }

    for (i = 0; i < nfds; i++) {
        if (rblio[i]) {
            sum += useIfPtr(&ioPool[i], f);
        }
    }

    sum += useIfPtr(&extAppCBRegistry, f);

    return sum;
}


int VirtualMachine::traversePtrs(SI__PSOb f) {
    int sum = 0;

    sum += useIfPtr(code, f);
    sum += useIfPtr(ctxt, f);
    sum += useIfPtr(upcallCtxt, f);
    sum += useIfPtr(systemMonitor, f);
    sum += useIfPtr(currentMonitor, f);
    sum += sleeperPool->traversePtrs(f);
    sum += strandPool->traversePtrs(f);
    int i = 0;
    for (; i < NSIG; i++) {
        sum += useIfPtr(sigPool[i], f);
    }

    for (i = 0; i < nfds; i++) {
        if (rblio[i]) {
            sum += useIfPtr(ioPool[i], f);
        }
    }

    sum += useIfPtr(extAppCBRegistry, f);

    return sum;
}


void VirtualMachine::traversePtrs(V__PSOb f) {
    useIfPtr(code, f);
    useIfPtr(ctxt, f);
    useIfPtr(upcallCtxt, f);
    useIfPtr(systemMonitor, f);
    useIfPtr(currentMonitor, f);
    sleeperPool->traversePtrs(f);
    strandPool->traversePtrs(f);
    int i = 0;
    for (; i < NSIG; i++) {
        useIfPtr(sigPool[i], f);
    }

    for (i = 0; i < nfds; i++) {
        if (rblio[i]) {
            useIfPtr(ioPool[i], f);
        }
    }
    useIfPtr(extAppCBRegistry, f);
}


int VirtualMachine::obSize() {
    int sz = traversePtrs(MF_ADDR(Ob::size));
    traversePtrs(MF_ADDR(Ob::unvisit));
    return sz;
}


int VirtualMachine::obCount() {
    int cnt = traversePtrs(MF_ADDR(Ob::obCount));
    traversePtrs(MF_ADDR(Ob::unvisit));
    return cnt;
}


void VirtualMachine::preScavenge() {
    if (IS_PTR(code)) {
        pc.relative = code->relativize(pc.absolute);
    }

    if (currentMonitor != INVALID) {
        currentMonitor->timer->setMode(tmGC);
    }
}


void VirtualMachine::scavenge() { traversePtrs(MF_ADDR(Ob::relocate)); }


void VirtualMachine::postScavenge() {
    if (IS_PTR(code)) {
        pc.absolute = code->absolutize(pc.relative);
    }

    if (currentMonitor != INVALID) {
        currentMonitor->timer->setMode(tmUser);
        bytecodes = &currentMonitor->opcodeCounts->word(0);
        Base::obCounts = &currentMonitor->obCounts->word(0);
    }
}


void VirtualMachine::mark() { traversePtrs(MF_ADDR(Ob::mark)); }


void VirtualMachine::check() { traversePtrs(MF_ADDR(Ob::checkOb)); }


void VirtualMachine::handleException(Ob* v, Instr instr, Location tag) {
    static const int ApplyPrimFamily = opApplyPrimTag & 0xf0;

    int family = OP_f0_opcode(instr) & 0xf0;

    switch (SYSVAL(v)) {
    case syscodeInvalid:
        /* We don't do diddly */
        break;
    case syscodeUpcall:
        if (family == ApplyPrimFamily) {
            handleApplyPrimUpcall(instr, tag);
        } else {
            handleXmitUpcall(instr, tag);
        }
        break;
    case syscodeSuspended:
        if (family == ApplyPrimFamily) {
            handleApplyPrimSuspend(instr);
        }
        break;
    case syscodeInterrupt:
        suicide("what to do with syscodeInterrupt?");
        break;
    case syscodeSleep:
        handleSleep();
        break;
    case syscodeDeadThread:
        /* We don't do diddly */
        break;
    default:
        suicide("unknown SysCode value (%d)", SYSVAL(v));
    }
}


void VirtualMachine::handleXmitUpcall(Instr instr, Location tag) {
    CodeBuf* cb = CodeBuf::create();
    PROTECT(cb);

    switch (OP_f0_opcode(instr)) {
    case opXmit | NextOff:
    case opXmit | NextOn:
    case opXmitTag | NextOff:
    case opXmitTag | NextOn:
    case opXmitArg | NextOff:
    case opXmitArg | NextOn:
    case opXmitReg | NextOff:
    case opXmitReg | NextOn:
        ASSIGN(upcallCtxt, ctxt, ctxt->ctxt);
        upcallCtxt->tag = tag;
        break;
    case opSend | NextOff:
    case opSend | NextOn:
        ASSIGN(upcallCtxt, ctxt, (pCtxt)NIV);
        break;
    default:
        suicide("unanticipated case in VirtualMachine::handleXmitUpcall");
    }

    upcallCtxt = (pCtxt)INVALID;
}


void VirtualMachine::handleApplyPrimUpcall(Instr instr, Location tag) {
    Tuple* litvec = NIL;
    PROTECT(litvec);
    CodeBuf* cb = CodeBuf::create();
    PROTECT(cb);

    switch (OP_f0_opcode(instr)) {
    case opApplyPrimTag | NextOff:
    case opApplyPrimTag | NextOn:
    case opApplyPrimArg | NextOff:
    case opApplyPrimArg | NextOn:
    case opApplyPrimReg | NextOff:
    case opApplyPrimReg | NextOn:
        cb->emitF5(opUpcallRtn, false, OP_f5_next(instr), 0);
        litvec = Tuple::create(1, tag.atom);
        break;
    case opApplyCmd | NextOff:
        cb->emitF0(opUpcallResume);
        break;
    case opApplyCmd | NextOn:
        cb->emitF0(opNxt);
        break;
    default:
        suicide("unanticipated case in VirtualMachine::handleApplyPrimUpcall");
    }

    /*
     * The ctxt pointed to by upcallCtxt is assumed to have already been
     * scheduled, presumable as a side effect of invoking some dispatch
     * member function (e.g., StdOprn::dispatch).
     */

    ctxt->pc = code->relativize(pc.absolute);
    Code* wrapper_code = Code::create(cb, litvec);
    pCtxt wrapper_ctxt = Ctxt::create(wrapper_code, NIL, ctxt, 1);
    ASSIGN(upcallCtxt, ctxt, wrapper_ctxt);
    upcallCtxt->tag = CtxtReg(CRN_Rslt);
    upcallCtxt = (pCtxt)INVALID;
}


void VirtualMachine::handleApplyPrimSuspend(Instr instr) {
    assert(!OP_f5_next(instr));
    ctxt->pc = code->relativize(pc.absolute);
}


void VirtualMachine::handleMissingBinding(pOb symbol, Location) {
    ctxt->missingBindingError(symbol);
}


void VirtualMachine::handleFormalsMismatch(Template* formals) {
    PROTECT(formals);
    ctxt->prepare();

    Tuple* new_argvec = Tuple::create(2, NIV);
    new_argvec->elem(0) = formals;
    new_argvec->elem(1) = ctxt;

    pCtxt new_ctxt = Ctxt::create(oprnFormalsMismatch, new_argvec);
    BASE(oprnFormalsMismatch)->dispatch(new_ctxt);
}


void VirtualMachine::handleVirtualMachineError() { ctxt->vmError(); }


void VirtualMachine::handleSleep() {
    ctxt->pc = code->relativize(pc.absolute);
    sleeperPool->push(ctxt);
}

void VirtualMachine::handleSignal() {
    sigset_t oldmask;
    sigset_t blockmask;

    sigemptyset(&blockmask);
    sigaddset(&blockmask, SIGIO);
    if (sigprocmask(SIG_BLOCK, &blockmask, &oldmask) < 0) {
        warning("Unable to block SIGIO...\n");
        reset(); /* clears sigvec */
        return;
    }

    /*
     * Can't override the SIGINT and SIGIO signals with a Rosette handler.
     */

    if (sigvec & sigmask(SIGINT)) {
        warning("Can't override the SIGINT and SIGIO signals\n");
        reset();                                        /* clears sigvec */
        (void)sigprocmask(SIG_SETMASK, &oldmask, NULL); /* enable interrupts */
        return;
    }

    if (sigvec & sigmask(SIGIO)) {
    /*
     * For each channel ready for io, call its handler.
     */

    retry_select:  // TODO: Get rid of the evil goto!!
        fd_set rfds = fds;
        FD_CLR(1, &rfds);
        FD_CLR(2, &rfds);  // remove stdout & stderr
        fd_set wfds;
        FD_ZERO(&wfds);  // don't look for writes
        fd_set efds = fds;

        sigvec &= ~sigmask(SIGIO);

        int n = select(nfds, &rfds, &wfds, &efds, &timeout);

        if (n > 0) {
            int i = 0;
            for (i = 0; i < nfds; i++) {
                if (FD_ISSET(i, &rfds)) {
                    (*ioFn[i])(VM_READ_EVENT, i, ioPool[i]);
                }
            }

            for (i = 0; i < nfds; i++) {
                if (FD_ISSET(i, &efds)) {
                    (*ioFn[i])(VM_EXCEPTION_EVENT, i, ioPool[i]);
                }
            }
        } else if (n < 0) {
            switch (errno) {
            case EINTR:
                goto retry_select;  // TODO: Get rid of the evil goto!!

            case EINVAL:
            case EFAULT:
                warning(sys_errmsg());
                exit(0);

            case EBADF: {
                /* ferret out bad fd in fds */
                fd_set tfds;
                FD_ZERO(&tfds);
                for (int i = 0; i < nfds; i++) {
                    if (FD_ISSET(i, &fds)) {
                        FD_SET(i, &tfds);
                        if (select(nfds, &tfds, &wfds, &wfds, &timeout) < 0 &&
                            errno == EBADF) {
                            warning("clearing file descriptor %d", i);
                            FD_CLR(i, &fds);
                            deleteIoHandler(i);
                        }

                        FD_ZERO(&tfds);
                    }
                    goto retry_select;
                }
            }
            default:
                warning(sys_errmsg());
                break;
            }
        }
    }

    for (int sig = 0; sigvec != 0 && sig < NSIG; sig++) {
        if (sigvec & sigmask(sig)) {
            sigvec &= ~sigmask(sig);
            (void)initiateRosetteSignal(sig);
        }
    }

    if (sigprocmask(SIG_SETMASK, &oldmask, NULL) < 0) {  // enable interrupts
        warning("Restoring blocked IO signal mask");
    }
}


pOb VirtualMachine::unwindAndApplyPrim(Prim* prim) {
    PROTECT(prim);
    Tuple* old_argvec = ctxt->argvec;
    PROTECT(old_argvec);
    int old_nargs = ctxt->nargs;
    Tuple* new_argvec = NIL;
    Tuple* suffix = (Tuple*)ctxt->arg(ctxt->nargs);
    if (ctxt->nargs != 0 || suffix != NIL) {
        if (!IS_A(suffix, Tuple)) {
            return prim->runtimeError(ctxt, "&rest value is not a tuple");
        }

        new_argvec = Tuple::create(ctxt->nargs + suffix->numberOfElements(),
                                   ctxt->nargs, suffix);
        for (int i = ctxt->nargs; i--;) {
            new_argvec->elem(i) = old_argvec->elem(i);
        }
    }

    /*
     * This code protects the current argvec, temporarily replacing it
     * with the unwound argvec for use by the primitive, and then
     * restoring it after the primitive has finished.  This is necessary
     * because of the way that the compiler permits inlined primitives
     * (the subjects of opApplyPrim opcodes) to share a common argvec.
     * Unwinding cannot be permitted to clobber the argvec that the
     * compiler has set up, or bad things can happen (and they are *hard*
     * to track down).
     */

    ASSIGN(ctxt, argvec, new_argvec);
    ctxt->nargs = new_argvec->numberOfElements();
    pOb result = prim->dispatchHelper(ctxt);
    ASSIGN(ctxt, argvec, old_argvec);
    ctxt->nargs = old_nargs;
    return result;
}


pOb VirtualMachine::unwindAndDispatch() {
    Tuple* new_argvec = NIL;
    Tuple* suffix = (Tuple*)ctxt->arg(ctxt->nargs);
    if (ctxt->nargs != 0 || suffix != NIL) {
        if (!IS_A(suffix, Tuple)) {
            return BASE(ctxt->trgt)
                ->runtimeError(ctxt, "&rest value is not a tuple");
        }

        new_argvec = Tuple::create(ctxt->nargs + suffix->numberOfElements(),
                                   ctxt->nargs, suffix);
        for (int i = ctxt->nargs; i--;) {
            new_argvec->elem(i) = ctxt->arg(i);
        }
    }

    /*
     * This code doesn't need to preserve the argvec, because the
     * compiler assumes that xmits (and sends) "own" the argvec and ctxt
     * that are provided to them.  In other words, no other code (other
     * than the code that will be invoked by the dispatch) will be using
     * this argvec, so there is no way to get in trouble by clobbering
     * it.
     */

    ASSIGN(ctxt, argvec, new_argvec);
    ctxt->nargs = new_argvec->numberOfElements();
    return BASE(ctxt->trgt)->dispatch(ctxt);
}


void VirtualMachine::traceOpcode() {
    char buf[128];
    code->dumpInstr(pc.absolute, &buf[0]);
    printf("%s\n", buf);
}


void VirtualMachine::load(pOb expr) {
    Code* cp = BASE(expr)->compileWrt(TopEnv, NIV);
    if (cp != INVALID) {
        setup(cp, nxtCode, LocRslt);
        execute();
    }
}


void VirtualMachine::evaluate(pOb expr) {
    Code* cp = BASE(expr)->compileWrt(TopEnv, NIV);
    if (cp != INVALID) {
        setup(cp, printResult, ArgReg(0));
        execute();
    }
}


// This routine is a quick and dirty global environment lookup.  I think there
// may be a more efficient way to do this, but I haven't researched it yet.
int idxGlobalEnv(pOb key) {
    int size=TAGVAL(GlobalEnv->keyVec->indexedSize());
    const char *keyStr = BASE(key)->asCstring();

    for(int i = 0; i < size; i++) {
        const char * val = BASE(GlobalEnv->keyVec->nth(i))->asCstring();

        if (strcmp(keyStr, val) == 0) {
            return i;
        }
    }
//    if (VerboseFlag)
        warning("  %s NOT found in GlobalEnv!\n", keyStr);
    return -1;
}

#define FETCH (*pc.absolute++)
#define ARG_LIMIT (ctxt->argvec->numberOfElements())

Ob* VirtualMachine::vmLiterals[16] = {0};

void VirtualMachine::execute() {
    Instr instr;
    Location loc;
    pOb result = INVALID;

    if (VerboseFlag) fprintf(stderr, "\n%s\n", __PRETTY_FUNCTION__);
    
    collectExportCode(code);

nextop:

    if (sigvec != 0)
        handleSignal();
    if (debugging_level)
        traceOpcode();

    instr = FETCH;
    bytecodes[OP_f0_opcode(instr)]++;

    // if (VerboseFlag) {
    //     int opcode = OP_f0_opcode(instr);
    //     if (0 <= opcode && opcode < MaxOpcodes) {
    //         fprintf(stderr, "  %s:\n", opcodeStrings[opcode]);
    //     }
    // }

    switch (OP_f0_opcode(instr)) {
    case opHalt:
        warning("halting...");
        goto exit;

    case opPush:
        ctxt = Ctxt::create(NIL, ctxt);
        goto nextop;

    case opPop:
        ctxt = ctxt->ctxt;
        goto nextop;

    case opNargs:
        ctxt->nargs = OP_f0_op0(instr);
        goto nextop;

    case opAlloc: {
        Tuple* t = Tuple::create(OP_f0_op0(instr), NIV);
        ASSIGN(ctxt, argvec, t);
        goto nextop;
    }


    case opPushAlloc: {
        Tuple* t = Tuple::create(OP_f0_op0(instr), NIV);
        ctxt = Ctxt::create(t, ctxt);
        goto nextop;
    }


    case opExtend: {
        Template* formals = (Template*)code->lit(OP_f0_op0(instr));
        Tuple* actuals = (Tuple*)INVALID;
        {
            /*
             * This PROTECT is put inside this block to ensure that
             * it is properly undone (by exiting the block scope)
             * without relying on the C++ compiler to undo it before
             * following the goto's.
             */
            PROTECT(formals);
            actuals = formals->match(ctxt->argvec, ctxt->nargs);
        }

        if (actuals == INVALID) {
            handleFormalsMismatch(formals);
            goto doNextThread;
        } else {
            installEnv(BASE(ctxt->env)->extendWith(formals->keymeta, actuals));
            ctxt->nargs = 0;
            goto nextop;
        }
    }

    case opOutstanding | 0:
    case opOutstanding | 1:
    case opOutstanding | 2:
    case opOutstanding | 3:
        ctxt->pc = OP_f6_pc(instr);
        ctxt->outstanding = OP_e0_op0(FETCH);
        goto nextop;

    case opFork | 0:
    case opFork | 1:
    case opFork | 2:
    case opFork | 3: {
        /*
         * Since we don't know whether ctxt is synchronous or
         * asynchronous, we need to clone it rather than call some
         * explicit constructor.
         *
         * Further, we push the strand to (try to) force breadth
         * first execution
         */
        pCtxt new_ctxt = (pCtxt)ctxt->clone();
        new_ctxt->pc = OP_f6_pc(instr);
        strandPool->push(new_ctxt);
        /* new_ctxt->scheduleStrand(); */
        goto nextop;
    }

    case opXmitTag | NextOff | UnwindOff:
    case opXmitTag | NextOff | UnwindOn:
    case opXmitTag | NextOn | UnwindOff:
    case opXmitTag | NextOn | UnwindOn:
        ctxt->nargs = OP_f4_nargs(instr);
        ctxt->tag.atom = code->lit(OP_f4_op0(instr));

    doXmit:
        result = (OP_f4_unwind(instr) ? unwindAndDispatch()
                                      : BASE(ctxt->trgt)->dispatch(ctxt));
        if (result == DEADTHREAD) {
            goto doNextThread;
        }

        if (result != SUSPENDED && IS(OTsysval, result)) {
            /*
             * SUSPENDED is the usual case, and it requires no action.
             */
            handleException(result, instr, ctxt->tag);
            goto doNextThread;
        }

        if (OP_f4_next(instr)) {
            goto doNextThread;
        }

        goto nextop;

    case opXmitArg | NextOff | UnwindOff:
    case opXmitArg | NextOff | UnwindOn:
    case opXmitArg | NextOn | UnwindOff:
    case opXmitArg | NextOn | UnwindOn:
        ctxt->nargs = OP_f4_nargs(instr);
        ctxt->tag = ArgReg(OP_f4_op0(instr));
        goto doXmit;

    case opXmitReg | NextOff | UnwindOff:
    case opXmitReg | NextOff | UnwindOn:
    case opXmitReg | NextOn | UnwindOff:
    case opXmitReg | NextOn | UnwindOn:
        ctxt->nargs = OP_f4_nargs(instr);
        ctxt->tag = CtxtReg((CtxtRegName)OP_f4_op0(instr));
        goto doXmit;

    case opXmit | NextOff | UnwindOff:
    case opXmit | NextOff | UnwindOn:
    case opXmit | NextOn | UnwindOff:
    case opXmit | NextOn | UnwindOn:
        ctxt->nargs = OP_f5_op0(instr);
        goto doXmit;

    case opXmitTagXtnd | NextOff | UnwindOff:
    case opXmitTagXtnd | NextOff | UnwindOn:
    case opXmitTagXtnd | NextOn | UnwindOff:
    case opXmitTagXtnd | NextOn | UnwindOn:
        ctxt->nargs = OP_f5_op0(instr);
        ctxt->tag.atom = code->lit(OP_e0_op0(FETCH));
        goto doXmit;

    case opXmitArgXtnd | NextOff | UnwindOff:
    case opXmitArgXtnd | NextOff | UnwindOn:
    case opXmitArgXtnd | NextOn | UnwindOff:
    case opXmitArgXtnd | NextOn | UnwindOn:
        ctxt->nargs = OP_f5_op0(instr);
        ctxt->tag = ArgReg(OP_e0_op0(FETCH));
        goto doXmit;

    case opXmitRegXtnd | NextOff | UnwindOff:
    case opXmitRegXtnd | NextOff | UnwindOn:
    case opXmitRegXtnd | NextOn | UnwindOff:
    case opXmitRegXtnd | NextOn | UnwindOn:
        ctxt->nargs = OP_f5_op0(instr);
        ctxt->tag = CtxtReg((CtxtRegName)OP_e0_op0(FETCH));
        goto doXmit;

    case opSend | NextOff | UnwindOff:
    case opSend | NextOff | UnwindOn:
    case opSend | NextOn | UnwindOff:
    case opSend | NextOn | UnwindOn:
        ctxt->ctxt = (pCtxt)NIV;
        ctxt->nargs = OP_f5_op0(instr);
        ctxt->tag = LocLimbo;
        goto doXmit;

    case opApplyPrimTag | NextOff | UnwindOff:
    case opApplyPrimTag | NextOff | UnwindOn:
    case opApplyPrimTag | NextOn | UnwindOff:
    case opApplyPrimTag | NextOn | UnwindOn: {
        uint16_t ext = FETCH.word;
        unsigned p_num = WORD_OP_e0_op0(ext);
        if (p_num == 255) {
            const uint16_t ext1 = FETCH.word;
            p_num = ext1;
        }

        ctxt->nargs = OP_f5_op0(instr);
        Prim* const prim = Prim::nthPrim(p_num);
        loc.atom = code->lit(WORD_OP_e0_op1(ext));
        result = (OP_f5_unwind(instr) ? unwindAndApplyPrim(prim)
                                      : prim->dispatchHelper(ctxt));
        if (result == DEADTHREAD) {
            goto doNextThread;
        }

        if (IS(OTsysval, result)) {
            handleException(result, instr, loc);
            goto doNextThread;
        }

        if (store(loc, ctxt, result))
            goto vmError;
        if (OP_f5_next(instr))
            goto doNextThread;
        goto nextop;
    }

    case opApplyPrimArg | NextOff | UnwindOff:
    case opApplyPrimArg | NextOff | UnwindOn:
    case opApplyPrimArg | NextOn | UnwindOff:
    case opApplyPrimArg | NextOn | UnwindOn: {
        uint16_t ext = FETCH.word;
        unsigned p_num = WORD_OP_e0_op0(ext);
        if (p_num == 255) {
            const uint16_t ext1 = FETCH.word;
            p_num = ext1;
        }

        ctxt->nargs = OP_f5_op0(instr);
        Prim* const prim = Prim::nthPrim(p_num);
        const int argno = WORD_OP_e0_op1(ext);
        result = (OP_f5_unwind(instr) ? unwindAndApplyPrim(prim)
                                      : prim->dispatchHelper(ctxt));
        if (result == DEADTHREAD)
            goto doNextThread;
        if (IS(OTsysval, result)) {
            handleException(result, instr, ArgReg(argno));
            goto doNextThread;
        }

        if (argno >= ARG_LIMIT)
            goto vmError;
        ASSIGN(ctxt->argvec, elem(argno), result);
        if (OP_f5_next(instr))
            goto doNextThread;
        goto nextop;
    }

    case opApplyPrimReg | NextOff | UnwindOff:
    case opApplyPrimReg | NextOff | UnwindOn:
    case opApplyPrimReg | NextOn | UnwindOff:
    case opApplyPrimReg | NextOn | UnwindOn: {
        uint16_t ext = FETCH.word;
        unsigned p_num = WORD_OP_e0_op0(ext);
        if (p_num == 255) {
            const uint16_t ext1 = FETCH.word;
            p_num = ext1;
        }

        ctxt->nargs = OP_f5_op0(instr);
        Prim* const prim = Prim::nthPrim(p_num);
        const int regno = WORD_OP_e0_op1(ext);
        result = (OP_f5_unwind(instr) ? unwindAndApplyPrim(prim)
                                      : prim->dispatchHelper(ctxt));
        if (result == DEADTHREAD)
            goto doNextThread;
        if (IS(OTsysval, result)) {
            handleException(result, instr, CtxtReg((CtxtRegName)regno));
            goto doNextThread;
        }

        ASSIGN(ctxt, reg(regno), result);
        if (OP_f5_next(instr))
            goto doNextThread;
        goto nextop;
    }

    case opApplyCmd | NextOff | UnwindOff:
    case opApplyCmd | NextOff | UnwindOn:
    case opApplyCmd | NextOn | UnwindOff:
    case opApplyCmd | NextOn | UnwindOn: {
        uint16_t ext = FETCH.word;
        unsigned p_num = WORD_OP_e0_op0(ext);
        if (p_num == 255) {
            const uint16_t ext1 = FETCH.word;
            p_num = ext1;
        }

        ctxt->nargs = OP_f5_op0(instr);
        Prim* const prim = Prim::nthPrim(p_num);
        result = (OP_f5_unwind(instr) ? unwindAndApplyPrim(prim)
                                      : prim->dispatchHelper(ctxt));
        if (result == DEADTHREAD)
            goto doNextThread;
        if (IS(OTsysval, result)) {
            handleException(result, instr, LocLimbo);
            goto doNextThread;
        }
        if (OP_f5_next(instr))
            goto doNextThread;
        goto nextop;
    }


    case opRtnTag | NextOff:
    case opRtnTag | NextOn:
        ctxt->tag.atom = code->lit(OP_f5_op0(instr));
    /* fall through */

    case opRtn | NextOff:
    case opRtn | NextOn:
    doRtn:
        if (ctxt->ret(ctxt->rslt))
            goto vmError;
        if (OP_f5_next(instr)) {
            goto doNextThread;
        } else {
            goto nextop;
        }

    case opRtnArg | NextOff:
    case opRtnArg | NextOn:
        ctxt->tag = ArgReg(OP_f5_op0(instr));
        goto doRtn;

    case opRtnReg | NextOff:
    case opRtnReg | NextOn:
        ctxt->tag = CtxtReg((CtxtRegName)OP_f5_op0(instr));
        goto doRtn;


    case opUpcallRtn | NextOff:
    case opUpcallRtn | NextOn:
        ctxt->tag.atom = code->lit(OP_f0_op0(instr));
        if (store(ctxt->tag, ctxt->ctxt, ctxt->rslt)) {
            goto vmError;
        }
        if (OP_f5_next(instr)) {
            goto doNextThread;
        }
    /* fall through */


    case opUpcallResume:
        ctxt->ctxt->scheduleStrand();
        goto doNextThread;


    case opNxt:
    doNextThread:
        if (getNextStrand())
            goto exit;
        goto nextop;


    case opJmpCut | 0:
    case opJmpCut | 1:
    case opJmpCut | 2:
    case opJmpCut | 3: {
        int cut = OP_e0_op0(FETCH);
        pOb new_env = ctxt->env;
        while (cut--) {
            new_env = new_env->parent();
        }
        ASSIGN(ctxt, env, new_env);
        /* fall through */
    }


    case opJmp | 0:
    case opJmp | 1:
    case opJmp | 2:
    case opJmp | 3:
        pc.absolute = code->absolutize(OP_f6_pc(instr));
        goto nextop;


    case opJmpFalse | 0:
    case opJmpFalse | 1:
    case opJmpFalse | 2:
    case opJmpFalse | 3:
        if (ctxt->rslt == RBLFALSE) {
            pc.absolute = code->absolutize(OP_f6_pc(instr));
        }
        goto nextop;


    case opLookupToArg | 0x0:
    case opLookupToArg | 0x1:
    case opLookupToArg | 0x2:
    case opLookupToArg | 0x3:
    case opLookupToArg | 0x4:
    case opLookupToArg | 0x5:
    case opLookupToArg | 0x6:
    case opLookupToArg | 0x7:
    case opLookupToArg | 0x8:
    case opLookupToArg | 0x9:
    case opLookupToArg | 0xa:
    case opLookupToArg | 0xb:
    case opLookupToArg | 0xc:
    case opLookupToArg | 0xd:
    case opLookupToArg | 0xe:
    case opLookupToArg | 0xf: {
        const int argno = OP_f2_op0(instr);
        int index = OP_f2_op1(instr);

        // Extract the deferred lookup bit from the offset index
        bool deferredLookup = ((index & CompilationUnit::LookupDeferMask) != 0);
        index &= ~CompilationUnit::LookupDeferMask;

        pOb key = code->lit(index);
        pOb val = BASE(BASE(ctxt->selfEnv)->meta())
                            ->lookupOBO(ctxt->selfEnv, key, ctxt);
        if (val == UPCALL) {
            ctxt->pc = code->relativize(pc.absolute);
            goto doNextThread;
        } else if (val == ABSENT) {
            // If the deferred lookup bit was set, this is a deferred
            // lookup that was emitted on purpose by the compiler. In this
            // case we need to also check the global environment for the symbol.
            if (deferredLookup) {
                if (VerboseFlag) fprintf(stderr, "  Deferred symbol lookup '%s'\n", BASE(key)->asCstring());
                int idx = idxGlobalEnv(key);
                if (idx >= 0) {
                    val = GlobalEnv->entry(idx);
                }
            } else {
                warning("Absent but not deferred symbol lookup '%s'", BASE(key)->asCstring());
            }

            if (val == ABSENT) {
                handleMissingBinding(key, ArgReg(argno));
                goto doNextThread;
            }
        }

        ASSIGN(ctxt->argvec, elem(argno), val);
        goto nextop;
    }


    case opLookupToReg | 0x0:
    case opLookupToReg | 0x1:
    case opLookupToReg | 0x2:
    case opLookupToReg | 0x3:
    case opLookupToReg | 0x4:
    case opLookupToReg | 0x5:
    case opLookupToReg | 0x6:
    case opLookupToReg | 0x7:
    case opLookupToReg | 0x8:
    case opLookupToReg | 0x9:
    case opLookupToReg | 0xa:
    case opLookupToReg | 0xb:
    case opLookupToReg | 0xc:
    case opLookupToReg | 0xd:
    case opLookupToReg | 0xe:
    case opLookupToReg | 0xf: {
        const int regno = OP_f2_op0(instr);
        int index = OP_f2_op1(instr);

        // Extract the deferred lookup bit from the offset index
        bool deferredLookup = ((index & CompilationUnit::LookupDeferMask) != 0);
        index &= ~CompilationUnit::LookupDeferMask;

        pOb key = code->lit(index);
        pOb val = BASE(BASE(ctxt->selfEnv)->meta())
                            ->lookupOBO(ctxt->selfEnv, key, ctxt);
        if (val == UPCALL) {
            ctxt->pc = code->relativize(pc.absolute);
            goto doNextThread;
        } else if (val == ABSENT) {
            // If the deferred lookup bit was set, this is a deferred
            // lookup that was emitted on purpose by the compiler. In this
            // case we need to also check the global environment for the symbol.
            if (deferredLookup) {
                if (VerboseFlag) fprintf(stderr, "  Deferred symbol lookup '%s'\n", BASE(key)->asCstring());
                int idx = idxGlobalEnv(key);
                if (idx >= 0) {
                    val = GlobalEnv->entry(idx);
                }
            } else {
                warning("Absent but not deferred symbol lookup '%s'", BASE(key)->asCstring());
            }

            if (val == ABSENT) {
                handleMissingBinding(key, ArgReg(regno));
                goto doNextThread;
            }
        }

        ASSIGN(ctxt, reg(regno), val);
        goto nextop;
    }


    case opXferLexToArg | IndirectOff | 0:
        /*
         * This is a minor win for the incredibly common lex[0,?] case.
         */
        ASSIGN(ctxt->argvec, elem(OP_f7_op0(instr)),
               ctxt->env->slot(OP_f7_offset(instr)));
        goto nextop;

    case opXferLexToArg | IndirectOff | 1:
    case opXferLexToArg | IndirectOff | 2:
    case opXferLexToArg | IndirectOff | 3:
    case opXferLexToArg | IndirectOff | 4:
    case opXferLexToArg | IndirectOff | 5:
    case opXferLexToArg | IndirectOff | 6:
    case opXferLexToArg | IndirectOff | 7:
    case opXferLexToArg | IndirectOn | 0:
    case opXferLexToArg | IndirectOn | 1:
    case opXferLexToArg | IndirectOn | 2:
    case opXferLexToArg | IndirectOn | 3:
    case opXferLexToArg | IndirectOn | 4:
    case opXferLexToArg | IndirectOn | 5:
    case opXferLexToArg | IndirectOn | 6:
    case opXferLexToArg | IndirectOn | 7: {
        short level = OP_f7_level(instr);
        pOb env = ctxt->env;
        while (level--) {
            env = BASE(env)->parent();
        }

        if (OP_f7_indirect(instr)) {
            env = ((Actor*)env)->extension;
        }
        ASSIGN(ctxt->argvec, elem(OP_f7_op0(instr)),
               env->slot(OP_f7_offset(instr)));
        goto nextop;
    }


    case opXferLexToReg | IndirectOff | 0:
        ASSIGN(ctxt, reg(OP_f7_op0(instr)),
               ctxt->env->slot(OP_f7_offset(instr)));
        goto nextop;

    case opXferLexToReg | IndirectOff | 1:
    case opXferLexToReg | IndirectOff | 2:
    case opXferLexToReg | IndirectOff | 3:
    case opXferLexToReg | IndirectOff | 4:
    case opXferLexToReg | IndirectOff | 5:
    case opXferLexToReg | IndirectOff | 6:
    case opXferLexToReg | IndirectOff | 7:
    case opXferLexToReg | IndirectOn | 0:
    case opXferLexToReg | IndirectOn | 1:
    case opXferLexToReg | IndirectOn | 2:
    case opXferLexToReg | IndirectOn | 3:
    case opXferLexToReg | IndirectOn | 4:
    case opXferLexToReg | IndirectOn | 5:
    case opXferLexToReg | IndirectOn | 6:
    case opXferLexToReg | IndirectOn | 7: {
        short level = OP_f7_level(instr);
        pOb env = ctxt->env;
        while (level--) {
            env = BASE(env)->parent();
        }

        if (OP_f7_indirect(instr)) {
            env = ((Actor*)env)->extension;
        }

        ASSIGN(ctxt, reg(OP_f7_op0(instr)), env->slot(OP_f7_offset(instr)));
        goto nextop;
    }


    case opXferGlobalToArg:
        ASSIGN(ctxt->argvec, elem(OP_f0_op0(instr)),
               GlobalEnv->entry(OP_e1_op0(FETCH)));
        goto nextop;


    case opXferGlobalToReg:
        ASSIGN(ctxt, reg(OP_f0_op0(instr)), GlobalEnv->entry(OP_e1_op0(FETCH)));
        goto nextop;


    case opXferArgToArg:
        /*
         * There is no need to check whether argvec is old here, since we
         * are simply moving an already-accounted-for value to a
         * different location within the argvec.
         */
        ctxt->argvec->elem(OP_f1_op0(instr)) =
            ctxt->argvec->elem(OP_f1_op1(instr));
        goto nextop;


    case opXferRsltToArg:
        ASSIGN(ctxt->argvec, elem(OP_f0_op0(instr)), ctxt->rslt);
        goto nextop;

    case opXferArgToRslt:
        ASSIGN(ctxt, rslt, ctxt->argvec->elem(OP_f0_op0(instr)));
        goto nextop;

    case opXferRsltToReg:
        ctxt->reg(OP_f0_op0(instr)) = ctxt->rslt;
        goto nextop;

    case opXferRegToRslt:
        ctxt->rslt = ctxt->reg(OP_f0_op0(instr));
        goto nextop;

    case opXferRsltToDest:
        loc.atom = code->lit(OP_f0_op0(instr));
        if (store(loc, ctxt, ctxt->rslt)) {
            goto vmError;
        } else {
            goto nextop;
        }

    case opXferSrcToRslt:
        loc.atom = code->lit(OP_f0_op0(instr));
        ASSIGN(ctxt, rslt, fetch(loc, ctxt));
        goto nextop;


    case opIndLitToArg:
        ASSIGN(ctxt->argvec, elem(OP_f1_op0(instr)),
               code->lit(OP_f1_op1(instr)));
        goto nextop;


    case opIndLitToReg:
        ASSIGN(ctxt, reg(OP_f1_op0(instr)), code->lit(OP_f1_op1(instr)));
        goto nextop;


    case opIndLitToRslt:
        ASSIGN(ctxt, rslt, code->lit(OP_f0_op0(instr)));
        goto nextop;


    case opImmediateLitToArg | 0x0:
    case opImmediateLitToArg | 0x1:
    case opImmediateLitToArg | 0x2:
    case opImmediateLitToArg | 0x3:
    case opImmediateLitToArg | 0x4:
    case opImmediateLitToArg | 0x5:
    case opImmediateLitToArg | 0x6:
    case opImmediateLitToArg | 0x7:
    case opImmediateLitToArg | 0x8:
    case opImmediateLitToArg | 0x9:
    case opImmediateLitToArg | 0xa:
    case opImmediateLitToArg | 0xb:
        ctxt->argvec->elem(OP_f2_op1(instr)) = vmLiterals[OP_f2_op0(instr)];
        goto nextop;


    case opImmediateLitToReg | 0x0:
    case opImmediateLitToReg | 0x1:
    case opImmediateLitToReg | 0x2:
    case opImmediateLitToReg | 0x3:
    case opImmediateLitToReg | 0x4:
    case opImmediateLitToReg | 0x5:
    case opImmediateLitToReg | 0x6:
    case opImmediateLitToReg | 0x7:
    case opImmediateLitToReg | 0x8:
    case opImmediateLitToReg | 0x9:
    case opImmediateLitToReg | 0xa:
    case opImmediateLitToReg | 0xb:
        ctxt->reg(OP_f2_op1(instr)) = vmLiterals[OP_f2_op0(instr)];
        goto nextop;

    default:
        warning("illegal instruction (0x%.4x)", (int)instr.word);
        goto doNextThread;
    }


vmError:
    handleVirtualMachineError();
    goto doNextThread;


exit:
    wrapup();
}


Ob* VirtualMachine::upcall(Ctxt* k) {
    upcallCtxt = k;
    return UPCALL;
}


void VirtualMachine::scheduleStrand(pCtxt strand) { strandPool->enq(strand); }


int VirtualMachine::addSignalHandler(int sig, sighandler_t fn, Ob* ob) {
    auto oldFn = signal(sig, fn);
    if (SIG_ERR == oldFn) {
        return -1;
    } else {
        if (SIG_DFL == oldFn) {
            nsigs += (SIG_DFL != fn);
        } else {
            nsigs -= (SIG_DFL == fn);
        }
        sigPool[sig] = ob;
        return 0;
    }
}


int VirtualMachine::deleteSignalHandler(int sig) {
    return addSignalHandler(sig, SIG_DFL);
}


Ob* VirtualMachine::initiateRosetteSignal(int sig) {
    if (sigPool[sig] == INVALID) {
        warning("no Rosette signal handler installed for signal %d", sig);
        return INVALID;
    } else {
        extern StdOprn* oprnSignal;
        Tuple* argvec = Tuple::create(2, NIV);
        argvec->elem(0) = sigPool[sig];
        argvec->elem(1) = FIXNUM(sig);
        pCtxt msg = Ctxt::create(oprnSignal, argvec);
        return BASE(sigPool[sig])->receive(msg);
    }
}


void VirtualMachine::resetSignals() {
    /*
     * This expects to be invoked only at startup time, either from the
     * VirtualMachine constructor or from BigBang.  The sig handlers are
     * evidently reset to their default state whenever an executable is
     * started, so there is no point in trying to set them.
     */

    /*
     * A value of INVALID in the sigPool means that the corresponding
     * signal is not being caught by Rosette-level code.  Any other value
     * is assumed to be an object to which a "signal" message will be
     * sent when that signal is raised; a non-INVALID value also
     * indicates that the RosetteSignalHandler.
     *
     * ioPool and ioFn are used to deal with asynchronous io initiated by
     * the SIGIO signal (which will presumably not be altered by the
     * user).  ioFn is a vector of functions of type IO_HANDLER; when the
     * vm determines that io is possible on file descriptor n, it calls
     * (*ioFn[n])(n), and it is up to that function deal with the io.
     * One special IO_HANDLER, RosetteIoHandler, is predefined; when it
     * invoked it sends oprnResumeIO (the "resume-io" operations) to the
     * object in ioPool[n].  The primitive "io-catch" (similar to
     * "sig-catch") installs this handler.  For example,
     *
     * 	(io-catch 13 foo)
     *
     * will install RosetteIoHandler in ioFn[13] and put foo in
     * ioPoo[13].  When io is ready on file descriptor 13, the vm will
     * execute (eventually)
     *
     * 	(resume-io foo)
     */

    sigvec = 0;
    int i = 0;
    for (; i < NSIG; i++) {
        sigPool[i] = INVALID;
    }

    for (i = 0; i < FD_SETSIZE; i++) {
        ioFn[i] = 0;
    }

    for (i = 0; i < FD_SETSIZE; i++) {
        ioPool[i] = INVALID;
    }

    for (i = 0; i < FD_SETSIZE; i++) {
        rblio[i] = 0;
    }

    /*
     * nsigs tells us how many non-default signal handlers are installed.
     * When it is non-zero the virtual machine will suspend using
     * sigpause whenever it runs out of threads to execute; it is
     * presumed that one of the installed handlers will eventually
     * respond to some event by making one or more threads ready to run.
     * If nsigs is zero when the virtual machine runs out of threads, it
     * returns back out to the main driver loop and goes into its
     * equivalent of console mode.
     */

    nsigs = 0;

    /*
     * These file descriptor set fds records the file descriptors that are
     * involved in SIGIO io.  We have to keep "permanent" track of them
     * here because the select system call modifies the fd_sets that it
     * receives as args.  Consequently, we copy fds into temps just before
     * the select call, and pass the copies instead.  The variable nfds
     * records the number of file descriptors that we are concerned about
     * (actually, there may be fewer than nfds files of interest because
     * there may be "holes" in the allocation map, but we have to keep
     * track of the highest numbered file descriptor for select).
     *
     * The rblio array tells us whether the corresponding entry in ioPool
     * is a pointer to a Rosette object (and therefore needs to be
     * traversed during garbage collection) or a a generic pointer to
     * some other C structure.
     */

    /*
     * The variable timeout is permanently set to timeval 0; it is used
     * only to tell select to return immediately.
     */

    FD_ZERO(&fds);
    timerclear(&timeout);
    nfds = 0;
}


void VirtualMachine::addIoHandler(int fd, IO_HANDLER* fn, void* ob, int rbl) {
    /*
     * Be sure to inform the signal subsystem that we are now interested
     * in SIGIO events.
     */

    if (nfds == 0) {
        addSignalHandler(SIGIO, &RosetteSignalHandler);
#ifdef HANDLE_POLL_WITH_IO
        addSignalHandler(SIGPOLL, &RosetteSignalHandler);
#endif
#ifdef NEED_ALARM
        addSignalHandler(SIGALRM, &RosetteSignalHandler);
#endif
    }
    /*
     * Should this take care of async-ifying the indicated file?  It is
     * currently left to some external agent.
     */

    ioFn[fd] = fn;
    ioPool[fd] = (Ob*)ob;
    FD_SET(fd, &fds);
    rblio[fd] = (rbl != 0);
    if (fd >= nfds) {
        nfds = fd + 1;
    }
}


void VirtualMachine::deleteIoHandler(int fd) {
    if ((fd < 0) || (fd >= nfds)) {
        return;
    }

    ioFn[fd] = 0;
    ioPool[fd] = INVALID;
    rblio[fd] = 0;
    FD_CLR(fd, &fds);

    if (fd + 1 == nfds) {
        /*
         * Then fd is the highest numbered file descriptor being
         * considered for io through signals.  Since we're deleting it,
         * we need to determine the next highest numbered file
         * descriptor, which we do by running backwards through the file
         * descriptor set.
         */
        for (nfds--; nfds > 0; nfds--) {
            if (FD_ISSET(nfds - 1, &fds)) {
                break;
            }
        }
    }

    if (nfds == 0) {
        /*
         * If there are no more file descriptors with io handlers, we can
         * revert the signal handler to the default.
         */
        deleteSignalHandler(SIGIO);
#ifdef HANDLE_POLL_WITH_IO
        deleteSignalHandler(SIGPOLL);
#endif
    }
}


void VirtualMachine::enableIo(int fd) { FD_SET(fd, &fds); }


void VirtualMachine::disableIo(int fd) { FD_CLR(fd, &fds); }


Ob* VirtualMachine::initiateRosetteIo(int fd) {
    if (ioPool[fd] == INVALID || !rblio[fd]) {
        warning("mixup in io initiation on file descriptor %d", fd);
    }

    extern StdOprn* oprnResumeIO;
    Tuple* argvec = Tuple::create(1, ioPool[fd]);
    pCtxt msg = Ctxt::create(oprnResumeIO, argvec);
    return BASE(ioPool[fd])->receive(msg);
}


void VirtualMachine::printStats(FILE* f) { systemMonitor->printStats(f); }


pCtxt makePrinterCtxt(Code* cp) {
    Tuple* argvec = Tuple::create(1, NIV);
    pCtxt ctxt = Ctxt::create(cp, argvec, (pCtxt)NIV, 1);

    ctxt->rslt = NIV;
    ctxt->self2 = printer;
    ctxt->selfEnv = printer;
    ctxt->env = GlobalEnv;
    ctxt->rcvr = printer;

    return ctxt;
}


extern Prim* actorNextBang;
extern StdOprn* oprnPrint;
extern Prim* obDisplay;


int GLOBAL_OFFSET(char* name) {
    Location loc = GlobalEnv->lex(SYMBOL(name), 0);
    return (GET_GENERIC_TYPE(loc) != LT_LexVariable ||
                    GET_LEXVAR_LEVEL(loc) != 0
                ? -1
                : GET_LEXVAR_OFFSET(loc));
}


void VirtualMachine::initVmTables() {
    {
        pMeta meta = StdMeta::create(NIL, FIXNUM(0), RBLTRUE);
        PROTECT(meta);
        pExt ext = StdExtension::create(CLASS_META(StdExtension), TopSBO, 0);
        printer = heap->tenure(Actor::create(meta, TopSBO, ext));
    }

    CodeBuf* cb = CodeBuf::create();
    PROTECT(cb);

    Tuple* litvec = Tuple::create(2, NIV);
    PROTECT(litvec);

    {
        /* Build the argvec for the display primitive */
        Tuple* argvec = Tuple::create(1, RBLCHAR('\n'));
        ASSIGN(litvec, elem(0), argvec);
    }

    {
        /* Build the template for the extend op */
        TupleExpr* pat = TupleExpr::create(1);
        pat->elem(0) = SYMBOL("val");
        Template* templat = pat->makeTemplate();
        ASSIGN(litvec, elem(1), templat);
    }

    static const uint16_t StartLabel = 0;
    static const uint16_t ResumeLabel = 8;

    /*  0 */ cb->emitF6(opOutstanding, ResumeLabel);
    cb->emitE0(1, 0);
    /*  2 */ cb->emitF0(opExtend, 1);
    /*  3 */ cb->emitF0(opPushAlloc, 1);
    /*  4 */ cb->emitF7(opXferLexToArg, false, 0, 0, 0);
    /*  5 */ cb->emitF0(opXferGlobalToReg, CRN_Trgt);
    cb->emitE1(GLOBAL_OFFSET("print"));
    /*  7 */ cb->emitF4(opXmitReg, false, true, 1, CRN_Rslt);
    /*  8 */ cb->emitF1(opIndLitToReg, CRN_Argvec, 0);
    /*  9 */ cb->emitF5(opApplyPrimReg, false, false, 1);
    cb->emitE2(obDisplay->primNumber(), CRN_Rslt);
    /* 11 */ cb->emitF0(opAlloc, 1);
    /* 12 */ cb->emitF6(opOutstanding, StartLabel);
    cb->emitE0(1, 0);
    /* 14 */ cb->emitF0(opNxt);

    printResult = (Code*)heap->tenure(Code::create(cb, litvec));

    cb->clear();
    cb->emitF0(opNxt);
    nxtCode = (Code*)heap->tenure(Code::create(cb, NIL));

    cb->clear();
    cb->emitF5(opRtn, false, true);
    rtnNxtCode = (Code*)heap->tenure(Code::create(cb, NIL));

    vmLiterals[0x0] = FIXNUM(0);
    vmLiterals[0x1] = FIXNUM(1);
    vmLiterals[0x2] = FIXNUM(2);
    vmLiterals[0x3] = FIXNUM(3);
    vmLiterals[0x4] = FIXNUM(4);
    vmLiterals[0x5] = FIXNUM(5);
    vmLiterals[0x6] = FIXNUM(6);
    vmLiterals[0x7] = FIXNUM(7);
    vmLiterals[0x8] = RBLTRUE;
    vmLiterals[0x9] = RBLFALSE;
    vmLiterals[0xa] = NIL;
    vmLiterals[0xb] = NIV;
}


MODULE_INIT(VirtualMachine) { VirtualMachine::initVmTables(); }
