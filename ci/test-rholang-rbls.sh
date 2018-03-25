#!/usr/bin/env bash
export PATH=$PATH:$(pwd -P)/ci
source header.sh

export ESS_SYSDIR=${ROSETTE_ROOT_DIR}/result/lib/rosette
# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

test_rbl_pass () {
    rbl_file = $1
    ${ROSETTE_ROOT_DIR}/result/lib/rosette --verbose \
        --boot-dir ${ESS_SYSDIR} ${rbl_file} | grep ^Pass
}

for rbl_file in $( ls ${RHOLANG_ROOT_DIR}/tests/*.rbl ); do
    if [[ ! $(test_rbl_pass ${rbl_file}) ]]; then
        echo '[error] - rbl file ${rbl_file} did not return "Pass"'
        exit 1
    fi
done

for rbl_file in $( ls ${RHOLANG_ROOT_DIR}/failure_tests/*.rbl ); do
    if [[ $(test_rbl_pass ${rbl_file}) ]]; then
        echo '[error] - rbl failure test file ${rbl_file} returned "Pass"'
        exit 1
    fi
done
