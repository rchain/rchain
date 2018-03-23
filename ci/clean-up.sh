#!/usr/bin/env bash
source ci/header.sh

# Remove rholang rbls used in testing
rm -f ${rholang_root_dir}/tests/*.rbl
rm -f ${rholang_root_dir}/failure_tests/*.rbl
