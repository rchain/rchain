#! /usr/bin/env bash

# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

# Execute Rosette instance
# Execute Rosette instance for local build or NixOS
if [ -d "build.out" ]; then
    export ESS_SYSDIR="rbl/rosette"
    ./build.out/src/rosette --quiet --boot-dir ${ESS_SYSDIR} --boot boot.rbl $@
else
    export ESS_SYSDIR="$(readlink -f $(pwd)/result/lib/rosette)"
    ./result/bin/rosette --quiet --boot-dir ${ESS_SYSDIR} --boot boot.rbl $@
fi

