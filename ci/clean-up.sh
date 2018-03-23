#!/usr/bin/env bash
source ci/header.sh

# set -euxo pipefail
# rholang_root_dir="${TRAVIS_BUILD_DIR}/rholang"

# Remove rhoscala serialized rbls
rm -f ${rholang_root_dir}/tests/*.rbl
rm -f ${rholang_root_dir}/failure_tests/*.rbl
