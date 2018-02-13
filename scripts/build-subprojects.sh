#!/bin/bash

set -e

if [ -d "${SUBPROJECT}" -a -f "${SUBPROJECT}/build.sh" ]; then
    echo "${SUBPROJECT}/build.sh"
    (cd "${SUBPROJECT}"; bash ./build.sh)
elif [ -f "build.sbt" ]; then
    sbt -Dsbt.log.noformat=true "< scripts/sbt-subprojects"
    for sub in comm rholang roscala storage node; do
	(bash <(curl -s https://codecov.io/bash) -X gcov -s ./$sub -c -F $sub)
    done
else
    echo "No build/test files found!"
    exit 1
fi
