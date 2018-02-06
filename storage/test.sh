#! /usr/bin/env bash

set -euo pipefail

SBT_OPTS="${SBT_OPTS:-} -Dsbt.log.noformat=true"

run_tests ()
{
    sbt ${SBT_OPTS} clean coverage test coverageReport
}

if run_tests
then
    # Upload test coverage reports to CodeCov
    bash <(curl -s https://codecov.io/bash) -c -F storage
    exit 0
else
    # On failure, exit with exit status of last command (run_tests)
    exit $?
fi
