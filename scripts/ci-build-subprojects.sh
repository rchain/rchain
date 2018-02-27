#!/usr/bin/env bash
## Builds all RChain subprojects 

# Set BASH environment so it will properly fail throwing exit code
set -euxo pipefail

project_root=$(pwd)

subprojects="rosette core" 
for subproject in $subprojects; do
    if [ -d "${subproject}" -a -f "${subproject}/build.sh" ]; then
        echo "${subproject}/build.sh"
        (cd "${subproject}"; bash ./build.sh)
    elif [ -f "build.sbt" ]; then
        sbt -Dsbt.log.noformat=true clean bnfc:generate coverage test coverageReport
        # for sub in crypto comm rholang roscala storage node; do
        # (bash <(curl -s https://codecov.io/bash) -X gcov -s ./$sub -c -F $sub)
        # done
    else
        echo "No build/test files found!"
        exit 1
    fi
done 

## Remove temporary files 
rm -rf ${project_root}/crypto/secp256k1
