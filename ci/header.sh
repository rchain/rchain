#!/usr/bin/env bash
set -exo pipefail

if [ "$TRAVIS" = "true" ]; then 
    PROJECT_ROOT_DIR=${TRAVIS_BUILD_DIR}
else
    PROJECT_ROOT_DIR=$(pwd -P)
fi

## Detect if running in docker container - setup using sudo accordingly
if [[ $(cat /proc/self/cgroup  | grep docker) = *docker* ]]; then
    echo "Running in docker container!"
    sudo=""
else
    sudo="sudo"
fi

ROSETTE_ROOT_DIR=${PROJECT_ROOT_DIR}/rosette
RHOLANG_ROOT_DIR=${PROJECT_ROOT_DIR}/rholang

cd ${PROJECT_ROOT_DIR}
