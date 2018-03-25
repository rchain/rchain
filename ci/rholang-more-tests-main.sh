#!/usr/bin/env bash
export PATH=$PATH:$(pwd -P)/ci
source header.sh

${PROJECT_ROOT_DIR}/ci/build-ubuntu-rosette.sh
${PROJECT_ROOT_DIR}/ci/build-rhoscala-rbls.sh
${PROJECT_ROOT_DIR}/ci/test-rholang-rbls.sh
${PROJECT_ROOT_DIR}/ci/clean-up.sh

cd ${PROJECT_ROOT_DIR}
