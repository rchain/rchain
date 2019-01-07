#!/bin/bash -e

tag=latest
if [[ -n $DRONE_BUILD_NUMBER ]]; then
    # Mind our own business on Drone CI with concurrent jobs
    tag=DRONE-$DRONE_BUILD_NUMBER
fi

export DEFAULT_IMAGE=rchain-integration-tests:$tag

sed "s/rnode:latest/rnode:$tag/" Dockerfile | docker build --quiet --tag "$DEFAULT_IMAGE" --file - .

pipenv run py.test -v "$@"
