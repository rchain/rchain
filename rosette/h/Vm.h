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

#if !defined(_RBL_Vm_h)
#define _RBL_Vm_h

#include "rosette.h"
#include "Ob.h"
#include "Interrupt.h"
#include "Location.h"
#include "Opcode.h"
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>


class VirtualMachine;
class Timer;
class ObStk;
class ObQue;
class RblTable;

#define EXTERNAL_APP_CALLBACK_KEY_GEN 0;

class VirtualMachine : public RootSet {
    struct PC {
        int relative;
        Instr* absolute;
    };

    PC pc;
    Code* code;
    Ctxt* ctxt;
    ObQue* strandPool;
    ObStk* sleeperPool;
    Ctxt* upcallCtxt;

    uint32_t* bytecodes;

    static Ob* vmLiterals[16];

    int traversePtrs(PSOb__PSOb);
    int traversePtrs(SI__PSOb);
    void traversePtrs(V__PSOb);

    void installCtxt(Ctxt*);
    void installEnv(Ob*);
    void installCode(Code*, int = 0);
    void installMonitor(Monitor*);

    bool getNextStrand();

    void handleException(Ob*, Instr, Location);
    void handleXmitUpcall(Instr, Location);
    void handleApplyPrimUpcall(Instr, Location);
    void handleApplyPrimSuspend(Instr);
    void handleMissingBinding(Ob*, Location);
    void handleFormalsMismatch(Template*);
    void handleVirtualMachineError();
    void handleSleep();
    void handleSignal();
    Ob* unwindAndApplyPrim(Prim*);
    Ob* unwindAndDispatch();
    void traceOpcode();

    friend class Ctxt;
    friend class AttrNode;  // For access to vmLiterals.

   public:
    Monitor* systemMonitor;
    Monitor* currentMonitor;

    uint32_t sigvec;
    Ob* sigPool[NSIG];
    IO_HANDLER* ioFn[FD_SETSIZE];
    Ob* ioPool[FD_SETSIZE];
    char rblio[FD_SETSIZE];
    int nsigs;

    fd_set fds;
    struct timeval timeout;
    int nfds;

    /* External applications may call back to rosette to get some work */
    /* done. In order to allow this to happen, we provide a keyed      */
    /* dispatch mechanism. The issue is that if moveable rosette       */
    /* objects are stored in applications that the garbage collector   */
    /* doesn't know about, then the external application will be left  */
    /* with garbage to hand to rosette to get some processing done,    */
    /* resulting in strange behavior. To prevent this, we always hand  */
    /* external applications immovable objects, e.g. fixnums, using    */
    /* these immovable objects as keys to the actors that do the real  */
    /* work.                                                           */
    uint32_t extAppCBKeyGen;
    RblTable* extAppCBRegistry;

    VirtualMachine();
    virtual ~VirtualMachine();

    static void initVmTables();

    void reset();
    void setup(Code*, Code*, Location);
    void wrapup();
    int obSize();
    int obCount();
    void load(Ob*);
    void evaluate(Ob*);
    void execute();
    Ob* upcall(Ctxt*);

    void preScavenge();
    void scavenge();
    void postScavenge();
    void mark();
    void check();

    int addSignalHandler(int, sighandler_t, Ob* = INVALID);
    int deleteSignalHandler(int);
    void acceptSignal(int sig) { sigvec |= sigmask(sig); }
    Ob* initiateRosetteSignal(int);
    void resetSignals();

    void addIoHandler(int, IO_HANDLER*, void* = NULL, int = 0);
    void deleteIoHandler(int);
    Ob* initiateRosetteIo(int);
    void enableIo(int);
    void disableIo(int);

    void scheduleStrand(Ctxt*);

    void printStats(FILE*);
};


extern VirtualMachine* vm;

#endif
