#!/bin/bash

set -e
set -x

rm -rf build.out
rm -rf build.third-party

if [ ! -h third-party ] ; then {
        rm -rf third-party ;
} ; fi
