#/bin/bash

set -o nounset
set -o errexit

cd integration-testing
python3 -m pip install pipenv
pipenv sync
./run_tests.sh
