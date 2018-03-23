#!/usr/bin/env bash
source ci/header.sh

sbt -Dsbt.log.noformat=true clean bnfc:generate coverage test coverageReport rpm:packageBin debian:packageBin
# ${project_root_dir}/ci/prep-ubuntu-with-deps.sh
${project_root_dir}/ci/build-rosette.sh
${project_root_dir}/ci/build-rhoscala-rbls.sh
${project_root_dir}/ci/test-rhoscala-rbls.sh
${project_root_dir}/ci/clean-up.sh

cd ${project_root_dir}
