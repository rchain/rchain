add_library(rosettecore BaseSupp.cc Actor.cc RblAtom.cc BigBang.cc BinaryOb.cc Code.cc
    CommandLine.cc Compile.cc Cstruct.cc ForeignFun.cc Ctxt.cc Dynload.cc
    Dump-world.cc Expr.cc Heap.cc Interrupt.cc Labels.cc Location.cc Mbox.cc
    Meta.cc Method.cc ModuleInit.cc Monitor.cc MI.cc Number.cc Ob.cc
    Operation.cc Parser.cc Pattern.cc Prim.cc Proc.cc Queue.cc RBLstring.cc
    RBLstream.cc Reader.cc Table.cc Tuple.cc Vm.cc ObQue.cc ObStk.cc
    PtrCollect.cc ResizeAry.cc RblStack.cc StreamUtils.cc StringStore.cc
    Timer.cc misc.cc Timestamp.cc Opcode.cc regexp.cc rosette-config.cc)


add_executable(rosette main.cc)
target_link_libraries (rosette rosettecore ${LIBS})

