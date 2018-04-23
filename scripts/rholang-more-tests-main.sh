#!/usr/bin/env bash
export PATH=$PATH:$(pwd -P)/scripts
source header.sh

${PROJECT_ROOT_DIR}/scripts/build-rhoscala-rbls.sh
${PROJECT_ROOT_DIR}/scripts/test-rholang-rbls.sh
# Remove rholang rbls used in testing
rm -f ${RHOLANG_ROOT_DIR}/tests/*.rbl
rm -f ${RHOLANG_ROOT_DIR}/failure_tests/*.rbl

cd ${PROJECT_ROOT_DIR}
