cmake_minimum_required (VERSION 3.5.2 FATAL_ERROR)
project (rosette CXX)
set (rosette_VERSION_MAJOR 1)
set (rosette_VERSION_MINOR 0)
set (rosette_VERSION_PATCH 1)

enable_testing()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake/Modules")

include (CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)

if (COMPILER_SUPPORTS_CXX11)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
elseif (COMPILER_SUPPORTS_CXX0X)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
else ()
    message(STATUS "No C++11 support from compile!")
endif ()

# TODO(leaf): Currently forcing 32-bit mode. Rosette will not compile in
# 64-bit mode, because of all the ptr->int casting. This needs to be fixed.
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DGCC27X")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m32")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-write-strings")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-invalid-offsetof")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-delete-incomplete")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-declarations")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-variable")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sign-compare")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-init-self")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-int-to-pointer-cast")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-shift-count-overflow")

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DARCH_INC=linux.h")

include_directories("${PROJECT_SOURCE_DIR}/h")
include_directories("${PROJECT_SOURCE_DIR}/h/sys")
add_subdirectory (src)
add_subdirectory (rbl/rosette)

include_directories("${PROJECT_BINARY_DIR}")

