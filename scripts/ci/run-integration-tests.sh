#/bin/bash -ue

cd /tmp/rchain/integration-testing
python3 -m pip install pipenv
pipenv sync
./run_tests.sh
