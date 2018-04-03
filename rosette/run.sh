#! /usr/bin/env bash

# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

# Execute Rosette instance
export ESS_SYSDIR="rbl/rosette"

./build.out/src/rosette --quiet --boot-dir ${ESS_SYSDIR} --boot boot.rbl $@
