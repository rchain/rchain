#!/usr/bin/env bash
export PATH=$PATH:$(pwd -P)/scripts
source header.sh

## Build RBLs
sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate rholang/assembly

jar=$(ls -t ${RHOLANG_ROOT_DIR}/target/scala*/*.jar | head -1)
# the above is usually file like rholang/target/scala-2.12/rholang-assembly-0.1.0-SNAPSHOT.jar

for rho_file in $(ls ${RHOLANG_ROOT_DIR}/tests/*.rho); do
    rbl_file=$(echo ${rho_file} | cut -f 1 -d '.').rbl
    java -jar ${jar} ${rho_file}
done

for rho_file in $(ls ${RHOLANG_ROOT_DIR}/failure_tests/*.rho); do
    rbl_file=$(echo ${rho_file} | cut -f 1 -d '.').rbl
if ! java -jar ${jar} ${rho_file} ; then
        echo "[success] with ${rho_file} failure test"
    else
        echo "[error] Test failure. Failure test returned true."
        exit 1
    fi
done


## Test RBLs
export ESS_SYSDIR=${ROSETTE_ROOT_DIR}/result/lib/rosette
# Increase stack limit so that Rosette can load all files
ulimit -s unlimited

test_rbl_pass () {
    rbl_file=$1
    rosette --quiet ${rbl_file} | grep ^Pass
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
