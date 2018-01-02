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

#include "Interrupt.h"
#include "RblAtom.h"
#include "Ctxt.h"
#include "Operation.h"
#include "Prim.h"
#include "Tuple.h"
#include "Vm.h"

int interruptPending = 0;
int arithmeticException = 0;


void interruptHandler(int signal) {
    interruptPending++;
    vm->acceptSignal(SIGINT);

#ifdef REINSTALL_SIGNALS
    signal(SIGINT, &interruptHandler);
#endif
}


void exceptionHandler(int signal) {
    arithmeticException++;

#ifdef REINSTALL_SIGNALS
    signal(SIGFPE, &exceptionHandler);
#endif
}


void RosetteSignalHandler(int signal) {
#ifdef DEBUG_SIGNALS
    printf("*** rosette signal: sig %d\n", sig);
#endif

#ifdef NEED_ALARM
    if (sig == SIGIO || sig == SIGALRM) {
        sig = SIGIO;
        alarm(1);
    }
#endif

    vm->acceptSignal(signal);

#ifdef REINSTALL_SIGNALS
    signal(SIGIO, &RosetteSignalHandler);
#ifdef HANDLE_POLL_WITH_IO
    signal(SIGPOLL, &RosetteSignalHandler);
#endif
#ifdef NEED_ALARM
    signal(SIGALRM, &RosetteSignalHandler);
#endif
#endif
}


void handleInterrupts() {
    auto oldHandler = signal(SIGINT, &interruptHandler);
    auto oldExcpt = signal(SIGFPE, &exceptionHandler);

    if (oldHandler == SIG_ERR || oldExcpt == SIG_ERR) {
        suicide(sys_errmsg());
    }
}


void ignoreInterrupts() {
    auto oldHandler = signal(SIGINT, SIG_DFL);
    auto oldExcpt = signal(SIGFPE, SIG_DFL);

    if (oldHandler == SIG_ERR || oldExcpt == SIG_ERR) {
        suicide(sys_errmsg());
    }
}


void RosetteIoHandler(VM_EVENT type, int fd, void*) {
    static char* eventName[] = {"read", "write", "exception"};

#ifdef DEBUG_SIGNALS
    printf("*** rosette io: %s event on file %d\n", eventName[type], fd);
#endif

    if (type == VM_READ_EVENT) {
        vm->initiateRosetteIo(fd);
    } else {
        warning("ignoring %s event on file %d", eventName[type], fd);
    }
}


DEF("sig-catch", sigCatch, 2, 2) {
    CHECK_FIXNUM(0, sig);
    if (sig < 0 || sig >= NSIG) {
        return PRIM_ERROR("invalid signal number");
    }

    if (ARG(1) == NIV) {
        if (vm->deleteSignalHandler(sig) == -1) {
            return PRIM_ERROR(sys_errmsg());
        }
    } else {
        sighandler_t fn = &RosetteSignalHandler;
        if (vm->addSignalHandler(sig, fn, ARG(1)) == -1) {
            return PRIM_ERROR(sys_errmsg());
        }
    }

    return NIV;
}


DEF("io-catch", ioCatch, 2, 2) {
    CHECK_FIXNUM(0, fd);
    if (fd < 0 || fd > FD_SETSIZE) {
        return PRIM_ERROR("invalid file descriptor");
    }

    if (ARG(1) == NIV) {
        vm->deleteIoHandler(fd);
    } else {
        vm->addIoHandler(fd, &RosetteIoHandler, ARG(1), 1);
    }

    return NIV;
}


DEF_OPRN(Std, "signal", oprnSignal, obRuntimeError);

/* TimeSvcHandler */

DEF("prim-handle-alarm", obHandleAlarm, 2, 2) {
    return PRIM_ERROR("primitive alarm!");
}

DEF_OPRN(Std, "handle-alarm", oprnHandleAlarm, obHandleAlarm);

void timeServiceHandler(int signal) {
#if defined(sparc)
    Tuple* tshinfo = (Tuple*)(vm->sigPool[sig]);
    PROTECT(tshinfo);
    Tuple* newtshinfo = Tuple::create(1, NIV);
    PROTECT(newtshinfo);
    Tuple* sleeperinfo;
    PROTECT(sleeperinfo);
    Tuple* av;
    PROTECT(av);
    Ctxt* c;
    PROTECT(c);
    int curint = FIXVAL(tshinfo->nth(0));
    int minint = MAXINT;
    int tmp;
    newtshinfo->setNth(0, tshinfo->nth(0));

    if (NIV != tshinfo->nth(1)) {
        sleeperinfo = (Tuple*)(tshinfo->nth(1));
        if ((tmp = ((FIXVAL(sleeperinfo->nth(1))) - curint)) <= 0) {
            /* send alarm to actor */
            av = Tuple::create(2, NIV);
            c = Ctxt::create(oprnHandleAlarm, av);

            if (IS_A(sleeperinfo->nth(0), Tuple)) {
                ASSIGN(av, elem(0), sleeperinfo->nth(0)->nth(0));
                ASSIGN(av, elem(1), sleeperinfo->nth(0)->nth(1));
                BASE(sleeperinfo->nth(0)->nth(0))->receive(c);
            } else {
                ASSIGN(av, elem(1), sleeperinfo->nth(0));
                BASE(sleeperinfo->nth(0))->receive(c);
            }

            /* reset interval */
            sleeperinfo->setNth(1, sleeperinfo->nth(2));
            minint = FIXVAL(sleeperinfo->nth(2));
        } else {
            if (tmp < minint) {
                minint = tmp;
            }
            sleeperinfo->setNth(1, FIXNUM(tmp));
        }
        newtshinfo = rcons(newtshinfo, sleeperinfo);
    } else {
        newtshinfo = rcons(newtshinfo, NIV);
    }
    for (int i = 2; i < tshinfo->numberOfElements(); i++) {
        sleeperinfo = (Tuple*)(tshinfo->nth(i));
        if ((tmp = (FIXVAL(sleeperinfo->nth(1)) - curint)) <= 0) {
            /* send alarm to actor */
            av = Tuple::create(2, NIV);
            c = Ctxt::create(oprnHandleAlarm, av);

            if (IS_A(sleeperinfo->nth(0), Tuple)) {
                ASSIGN(av, elem(0), sleeperinfo->nth(0)->nth(0));
                ASSIGN(av, elem(1), sleeperinfo->nth(0)->nth(1));
                BASE(sleeperinfo->nth(0)->nth(0))->receive(c);
            } else {
                ASSIGN(av, elem(1), sleeperinfo->nth(0));
                BASE(sleeperinfo->nth(0))->receive(c);
            }
        } else {
            if (tmp < minint) {
                minint = tmp;
            }

            sleeperinfo->setNth(1, FIXNUM(tmp));
            newtshinfo = rcons(newtshinfo, sleeperinfo);
        }
    }

    if ((newtshinfo->numberOfElements() > 2) || (newtshinfo->nth(1) != NIV)) {
        vm->sigPool[sig] = newtshinfo;
        /* issue the next ualarm call */
        (void)ualarm(minint, 0);
    } else { /* remove this handler from sighandlers */
        vm->deleteSignalHandler(sig);
    }

#endif
}

DEF("prim-register-alarm", registerAlarm, 3, 4) {
/* if handler already installed add [ARG(0) ARG(1)] to sigPool[SIGALRM]
else install handler and add etc. */

#if defined(sparc)

    Ob* beauty = ARG(0);
    PROTECT(beauty);
    if (!IS_FIXNUM(ARG(1))) {
        return PRIM_MISMATCH((1), "Fixnum");
    }

    CHECK(2, RblBool, periodicP);
    PROTECT(periodicP);

    Ob* svctag;
    PROTECT(svctag);

    if (NARGS == 4) {
        svctag = ARG(3);
        Tuple* actortagpair = Tuple::create(2, NIV);
        PROTECT(actortagpair);
        actortagpair->setNth(0, beauty);
        actortagpair->setNth(1, svctag);
        beauty = actortagpair;
    }

    Tuple* sleeper;
    if (vm->sigPool[SIGALRM] == INVALID) {
        Tuple* coccoon;
        auto fn = &timeServiceHandler;

        /* See timeServiceHandler for Data Structure setup.
           0th entry is the min interval.
           1st entry is the only periodic service.
           2nd - nth entry are non-periodic alarmists. */

        if (!(BOOLVAL(periodicP))) {
            sleeper = Tuple::create(2, NIV);
            sleeper->setNth(0, beauty);
            sleeper->setNth(1, ARG(1));
            PROTECT(sleeper);
            coccoon = Tuple::create(3, NIV);
            coccoon->setNth(0, ARG(1));
            coccoon->setNth(1, NIV);
            coccoon->setNth(2, sleeper);
        } else {
            sleeper = Tuple::create(3, NIV);
            sleeper->setNth(0, beauty);
            sleeper->setNth(1, ARG(1));
            sleeper->setNth(2, ARG(1));
            PROTECT(sleeper);
            coccoon = Tuple::create(2, NIV);
            coccoon->setNth(0, ARG(1));
            coccoon->setNth(1, sleeper);
        }

        if (vm->addSignalHandler(SIGALRM, fn, coccoon) == -1) {
            return PRIM_ERROR(sys_errmsg());
        }

        /* issue the very first ualarm call */
        (void)ualarm((FIXVAL(ARG(1))), 0);

    } else {
        if (BOOLVAL(periodicP)) {
            if (vm->sigPool[SIGALRM]->nth(1) != NIV) {
                PRIM_ERROR("must unregister periodic service first");
            }

            sleeper = Tuple::create(3, NIV);
            sleeper->setNth(0, beauty);
            sleeper->setNth(1, ARG(1));
            sleeper->setNth(2, ARG(2));
            vm->sigPool[SIGALRM]->setNth(1, sleeper);
        } else {
            /* Actually, this code should check to see if the ARG(1) < min */
            sleeper = Tuple::create(2, NIV);
            sleeper->setNth(0, beauty);
            sleeper->setNth(1, ARG(1));
            PROTECT(sleeper);
            vm->sigPool[SIGALRM] =
                rcons(((Tuple*)(vm->sigPool[SIGALRM])), sleeper);
        }
    }
#else
    /* defined(sgi) */
    PRIM_ERROR("time service not supported on this operating system");
#endif
    return NIV;
}

DEF("prim-unregister-alarm", unregisterAlarm, 1, 1) {
/* remove ARG(0) from vm->sigPool[SIGALRM]
  if vector drops to zero entries remove handler */

#if defined(sparc)

    Ob* beauty = ARG(0);
    PROTECT(beauty);
    if (vm->sigPool[SIGALRM] == INVALID) {
        PRIM_ERROR("nothing to remove");
    } else {
        /* this loop should be folded back up to simplify the code! */

        Tuple* tmp = (Tuple*)(vm->sigPool[SIGALRM]);
        PROTECT(tmp);
        Tuple* newtshinfo = Tuple::create(2, NIV);
        PROTECT(newtshinfo);
        newtshinfo->setNth(0, tmp->nth(0));

        int disjunct1, disjunct2, disjunct3a, disjunct3b;

        disjunct1 = (tmp->nth(1) == NIV);
        disjunct2 = (((Tuple*)(tmp->nth(1)))->nth(0) != beauty);
        disjunct3a = IS_A((((Tuple*)(tmp->nth(1)))->nth(0)), Tuple);
        disjunct3b =
            (((Tuple*)(((Tuple*)(tmp->nth(1)))->nth(0)))->nth(0) != beauty);

        if (disjunct1 || (disjunct2 && disjunct3a && disjunct3b)) {
            newtshinfo->setNth(1, tmp->nth(1));
            int flag = 0;
            for (int i = 2; i < tmp->numberOfElements(); i++) {
                disjunct2 = (((Tuple*)(tmp->nth(i)))->nth(0) != beauty);
                disjunct3a = IS_A((((Tuple*)(tmp->nth(i)))->nth(0)), Tuple);
                disjunct3b =
                    (((Tuple*)(((Tuple*)(tmp->nth(i)))->nth(0)))->nth(0) !=
                     beauty);

                if (disjunct2 || (disjunct3a && disjunct3b)) {
                    newtshinfo = rcons(newtshinfo, tmp->nth(i));
                } else {
                    flag = 1;
                }
            }

            if (!flag) {
                PRIM_ERROR("no service removed");
            }

            if ((newtshinfo->numberOfElements() == 2) &&
                (newtshinfo->nth(1) == NIV)) {
                ualarm(0, 0);
                vm->deleteSignalHandler(SIGALRM);
            } else {
                vm->sigPool[SIGALRM] = newtshinfo;
            }
        } else {
            if ((tmp->numberOfElements() == 2) && (tmp->nth(1) != NIV)) {
                ualarm(0, 0);
                vm->deleteSignalHandler(SIGALRM);
            } else {
                for (int i = 2; i < tmp->numberOfElements(); i++) {
                    newtshinfo = rcons(newtshinfo, tmp->nth(i));
                }
            }
        }
    }

#else
    PRIM_ERROR("time service not supported on this operating system");
#endif
    return NIV;
}
