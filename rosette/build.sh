#!/bin/bash
#
# Copyright (c) 2017, Pyrofex Corporation.
# Author: Nash E. Foster <leaf@pyrofex.net>
#
# Build third-party libraries.
#
set -e
set -x

THIRD_PARTY_BUILD="${1:-build.third-party}"
DEF_PFX="$(readlink -f $(pwd))/build.out"
BUILD="${2:-${DEF_PFX}}"
LIBDIR="${THIRD_PARTY_BUILD}/lib"
export VERBOSE=1
export DEBUG=1
CMAKE_VARS="-DCMAKE_BUILD_TYPE=${BUILD:-Release}"

SRCDIR=$(readlink -f $(pwd))
if [ ! -d ${BUILD} ] ; then {
    mkdir -p ${BUILD};
} ; fi

cd ${BUILD}
cmake -B${BUILD} -H${SRCDIR}
make -j 8
make test
# TODO(leaf): We need a package command. Not integrated yet, but
# leaving this breadcrumb to remind ourselves.
# make package

