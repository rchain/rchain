file(COPY apropos.rbl async-repl.rbl boot.rbl classes-types.rbl
    config.rbl configuration.rbl c-structures.rbl document.rbl
    dyn-load.rbl exceptions.rbl expander.rbl foreign-funs.rbl
    hello_world.rbl io-system.rbl meth-proc-oprns.rbl multimethod.rbl
    primitives.rbl que-stk-oprns.rbl sbo-init.rbl namespace-util.rbl 
    namespace.rbl consume.rbl produce.rbl
    string-oprns.rbl system.rbl table-oprns.rbl time.rbl timesvc.rbl
    trace.rbl tuple-oprns.rbl vm-heap.rbl log.rbl
    DESTINATION lib/rosette)

install(FILES apropos.rbl async-repl.rbl boot.rbl classes-types.rbl
    config.rbl configuration.rbl c-structures.rbl document.rbl
    dyn-load.rbl exceptions.rbl expander.rbl foreign-funs.rbl
    hello_world.rbl io-system.rbl meth-proc-oprns.rbl multimethod.rbl
    primitives.rbl que-stk-oprns.rbl sbo-init.rbl namespace-util.rbl
    namespace.rbl consume.rbl produce.rbl
    string-oprns.rbl system.rbl table-oprns.rbl time.rbl timesvc.rbl
    trace.rbl tuple-oprns.rbl vm-heap.rbl log.rbl
    DESTINATION lib/rosette)

add_test(NAME boot_rbl
    COMMAND rosette --boot-dir lib/rosette /dev/null)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/run-tests.ros
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/equiv.ros
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/reader-iso-8859-1.ros
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/reader-utf-8.ros
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/fold-tests.rbl
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/consume-tests.rbl
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/produce-tests.rbl
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/namespace-util-tests.rbl
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/transition-spec.rbl
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests)

add_test(NAME test_scripts
    COMMAND rosette --boot-dir lib/rosette tests/run-tests.ros)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tests/segfaults/regression-ros-304.rbl
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/tests/segfaults)

add_test(NAME regression_ros_304
    COMMAND rosette --boot-dir lib/rosette
    tests/segfaults/regression-ros-304.rbl)

