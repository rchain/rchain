# The protobuf calls only work properly within the same directory.

find_package(Protobuf REQUIRED)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS protobuf/Ob.proto)

add_executable(
    rosette main.cc Actor.cc RblAtom.cc BigBang.cc BinaryOb.cc Code.cc
    CommandLine.cc Compile.cc Cstruct.cc ForeignFun.cc Ctxt.cc Dynload.cc
    Dump-world.cc Expr.cc Heap.cc Interrupt.cc Labels.cc Location.cc Mbox.cc
    Meta.cc Method.cc ModuleInit.cc Monitor.cc MI.cc Number.cc Ob.cc
    Operation.cc Parser.cc Pattern.cc Prim.cc Proc.cc Queue.cc RBLstring.cc
    RBLstream.cc Reader.cc Table.cc Tuple.cc Vm.cc ObQue.cc ObStk.cc
    PtrCollect.cc ResizeAry.cc RblStack.cc StreamUtils.cc StringStore.cc
    Timer.cc misc.cc Timestamp.cc Opcode.cc main.cc BaseSupp.cc
    rosette-config.cc Export.cc Import.cc
    ${PROTO_SRCS} ${PROTO_HDRS}
    )

target_link_libraries (rosette ${PROTOBUF_LIBRARIES} ${LIBS})
install(TARGETS rosette RUNTIME DESTINATION bin
                        LIBRARY DESTINATION lib
                        ARCHIVE DESTINATION lib)
