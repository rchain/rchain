#!/usr/bin/env bash
source ci/header.sh

sbt -Dsbt.log.noformat=true clean bnfc:generate
sbt -Dsbt.log.noformat=true rholang/compile 
sbt -Dsbt.log.noformat=true rholang/assembly 
sbt -Dsbt.log.noformat=true rholangCLI/assembly

jar=$(ls -t ${rholang_root_dir}/target/scala*/*.jar | head -1)
# the above is usually file like rholang/target/scala-2.12/rholang-assembly-0.1.0-SNAPSHOT.jar

for rho_file in $(ls ${rholang_root_dir}/tests/*.rho); do
    rbl_file=$(echo ${rho_file} | cut -f 1 -d '.').rbl
    java -jar ${jar} ${rho_file}
    #rm ${rbl_file}
done

for rho_file in $(ls ${rholang_root_dir}/failure_tests/*.rho); do
    rbl_file=$(echo ${rho_file} | cut -f 1 -d '.').rbl
if ! java -jar ${jar} ${rho_file} ; then
        echo "[success] with ${rho_file} failure test"
        rm -f ${rbl_file}
    else
        echo "[error] Test failure. Failure test returned true."
        rm -f ${rbl_file}
        exit 1
    fi
done
