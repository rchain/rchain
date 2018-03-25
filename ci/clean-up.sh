#!/usr/bin/env bash
source ci/header.sh

# Remove rholang rbls used in testing
rm -f ${RHOLANG_ROOT_DIR}/tests/*.rbl
rm -f ${RHOLANG_ROOT_DIR}/failure_tests/*.rbl
