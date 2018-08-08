#!/usr/bin/env bash
# Prep "set" for CI if CI environment variable is set
if [[ "${CI}" = "true" ]]; then
    set -exo pipefail
else
    set
fi

if [[ -z "$PROJECT_ROOT_DIR" ]]; then
	PROJECT_ROOT_DIR="$(pwd -P)"
fi

## Prep Docker Image 
# BUILDER_DOCKER_NAME="rchain-builder-$(mktemp | awk -F. '{print $2}')"
# Use the above vs below if you want unique build names every time. 
# If build fails before last line with "rm -f" the container will stay running for more exploration. 
BUILDER_DOCKER_NAME="rchain-builder-tmp"

# If container exists force remove it.
if [[ $(docker ps -aq -f name=${BUILDER_DOCKER_NAME}) ]]; then
    docker rm -f ${BUILDER_DOCKER_NAME}
fi

echo "Running local build."
docker run -dit -v /var/run/docker.sock:/var/run/docker.sock \
	-v "$PROJECT_ROOT_DIR":/app \
    --name ${BUILDER_DOCKER_NAME} ubuntu:16.04
# Copy and run build and push docker script in docker builder container from above.
docker cp "$(dirname "$0")"/rnode-docker-build-script.sh ${BUILDER_DOCKER_NAME}:/ 
docker exec -it ${BUILDER_DOCKER_NAME} bash -c "./rnode-docker-build-script.sh"

# Clean Up
docker rm -f ${BUILDER_DOCKER_NAME}
