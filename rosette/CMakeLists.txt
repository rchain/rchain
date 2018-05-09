cmake_minimum_required (VERSION 3.5.1 FATAL_ERROR)
project (rosette CXX)
set (rosette_VERSION_MAJOR 4)
set (rosette_VERSION_MINOR 0)
set (rosette_VERSION_PATCH 0)

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
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-write-strings")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-invalid-offsetof")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-array-bounds")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-variable")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-result")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-uninitialized")

configure_file("${PROJECT_SOURCE_DIR}/h/config.h.in" "${PROJECT_BINARY_DIR}/include/config.h")
include_directories("${PROJECT_BINARY_DIR}/include")
include_directories("${PROJECT_SOURCE_DIR}/h")
include_directories("${PROJECT_SOURCE_DIR}/h/sys")
add_subdirectory(src)
add_subdirectory(rbl/rosette)

include_directories("${PROJECT_BINARY_DIR}")

INCLUDE(InstallRequiredSystemLibraries)

SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "The Rosette Virtual Machine")
SET(CPACK_PACKAGE_VENDOR "RChain Cooperative")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.txt")
SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/Copyright.txt")
SET(CPACK_PACKAGE_VERSION_MAJOR ${pyr8_VERSION_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${pyr8_VERSION_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${pyr8_VERSION_PATCH})
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "CMake ${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")

# Just Unix so far.
SET(CPACK_STRIP_FILES "build.out/src/rosette")
SET(CPACK_SOURCE_STRIP_FILES "")

SET(CPACK_PACKAGE_EXECUTABLES "build.out/src/rosette" "rosette")

SET(CPACK_GENERATOR "DEB")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "Nash Foster")
SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "amd64")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS
    "g++-multilib")

INCLUDE(CPack)

