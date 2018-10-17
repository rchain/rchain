#/bin/bash -ue

cd integration-testing
python3 -m pip install pipenv
pipenv sync
./run_tests.sh
