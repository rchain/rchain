#/bin/bash

set -o nounset
set -o errexit

RCHAIN_DIR="$PWD"
export CI_LOGS_DIR="/var/tmp/rchain-it/${DRONE_BUILD_NUMBER:-0}.$(date -Is)"
mkdir -p "$CI_LOGS_DIR"

cd integration-testing
python3 -m pip install pipenv
pipenv sync
./run_tests.sh
