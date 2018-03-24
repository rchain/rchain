#! /usr/bin/env bash

# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

# Execute Rosette instance
export ESS_SYSDIR="$(readlink -f $(pwd)/result/lib/rosette)"

./result/bin/rosette --verbose --boot-dir $ESS_SYSDIR --boot boot.rbl $@
