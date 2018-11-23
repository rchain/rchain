#!/bin/bash -ue

docker build --tag rchain-integration-testing .
pipenv run py.test -v "$@"
