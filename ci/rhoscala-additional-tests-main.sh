#!/usr/bin/env bash
source ci/header.sh

# ${project_root_dir}/ci/prep-ubuntu-with-deps.sh
${project_root_dir}/ci/build-rosette.sh
${project_root_dir}/ci/build-rhoscala-rbls.sh
${project_root_dir}/ci/test-rhoscala-rbls.sh
${project_root_dir}/ci/clean-up.sh

cd ${project_root_dir}
