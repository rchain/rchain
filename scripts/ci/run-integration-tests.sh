#/bin/bash

set -o nounset
set -o errexit

RCHAIN_DIR="$PWD"
export CI_LOGS_DIR="/var/tmp/rchain-it/${DRONE_BUILD_NUMBER:-0}.$(date -Is)"
mkdir -p "$CI_LOGS_DIR"

rm_ci_resources_dir () {
    [ -d "$CI_RESOURCES_DIR" ] && rm -rf "$CI_RESOURCES_DIR"
}
export CI_RESOURCES_DIR=$(mktemp --directory --tmpdir=/tmp integration-tests.${DRONE_BUILD_NUMBER:-0}.XXXXXXXXXX)
trap rm_ci_resources_dir EXIT
cp -r integration-testing/resources/* "$CI_RESOURCES_DIR/"

cd integration-testing
python3 -m pip install pipenv
pipenv sync
./run_tests.sh
