#!/usr/bin/env bash
export PATH=$PATH:$(pwd -P)/scripts
source header.sh

sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate rholang/assembly rholangCLI/assembly

rholang_jar=$(ls -t ${RHOLANG_ROOT_DIR}/target/scala*/*.jar | head -1)
# the above is usually file like rholang/target/scala-2.12/rholang-assembly-0.1.0-SNAPSHOT.jar

rholang_cli_jar=$(ls -t ${RHOLANG_CLI_ROOT_DIR}/target/scala*/*.jar | head -1)
# the above is usually file like rholang/target-cli/scala-2.12/rholangCLI-assembly-0.1.0-SNAPSHOT.jar

for rho_file in $(ls ${RHOLANG_ROOT_DIR}/tests/*.rho); do
    rbl_file=$(echo ${rho_file} | cut -f 1 -d '.').rbl
    java -jar ${rholang_jar} ${rho_file}
done

for rho_file in $(ls ${RHOLANG_ROOT_DIR}/failure_tests/*.rho); do
    rbl_file=$(echo ${rho_file} | cut -f 1 -d '.').rbl
if ! java -jar ${rholang_jar} ${rho_file} ; then
        echo "[success] with ${rho_file} failure test"
    else
        echo "[error] Test failure. Failure test returned true."
        exit 1
    fi
done

for rho_file in $(ls ${RHOLANG_ROOT_DIR}/tests/mercury-tut/*.rho); do
    rbl_file=$(echo ${rho_file} | cut -f 1 -d '.').rbl
    java -jar ${rholang_cli_jar} ${rho_file}
done
