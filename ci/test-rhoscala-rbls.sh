#!/usr/bin/env bash
source ci/header.sh

for rbl_file in $(ls ${rholang_root_dir}/tests/*.rbl); do
    out=$([[ $( ulimit -s unlimited && export ESS_SYSDIR=${rosette_root_dir}/rbl/rosette && ${rosette_root_dir}/build.out/src/rosette --quiet --boot-dir=${rosette_root_dir}/rbl/rosette --boot=boot.rbl ${rbl_file} | grep ^Pass ) ]] && echo true || echo false)
    if [[ ! $out = "true" ]]; then
        echo "[error] - rbl file ${rbl_file} did not return \"Pass\""
        exit 1
    fi
done

for rbl_file in $(ls ${rholang_root_dir}/failure_tests/*.rbl); do
    out=$([[ $( ulimit -s unlimited && export ESS_SYSDIR=${rosette_root_dir}/rbl/rosette && ${rosette_root_dir}/build.out/src/rosette --quiet --boot-dir=${rosette_root_dir}/rbl/rosette --boot=boot.rbl ${rbl_file} | grep ^Pass ) ]] && echo true || echo false)
    if [[ $out = "true" ]]; then
        echo "[error] - rbl failure test file ${rbl_file} returned \"Pass\""
        exit 1
    fi
done
