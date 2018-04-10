#!/usr/bin/env bash
# Prep "set" for CI if CI environment variable is set
if [[ "${CI}" = "true" ]]; then
    set -exo pipefail
else
    set -e
fi

# Receive Params and Set Variables
if [[ "${TRAVIS}" = "true" ]]; then
    echo "Running build on Travis CI"
    branch_name="dev"
    git_repo="https://github.com/rchain/rchain"
    docker_dst_repo="rchain/rnode:${TRAVIS_BRANCH}"
elif [[ $1 && $2 && $3 ]]; then
    echo "Running custom build"
    branch_name=$1
    git_repo=$2
    docker_dst_repo="$3"
else
    echo "Invalid number of parameters."
    echo "Usage: $0 <branch name> <repo url> <docker hub repo:tag>"
    echo "Usage: $0 dev https://github.com/rchain/rchain myrepo/rnode:mytagname"
    echo "You will be asked for you Docker repo user/password." 
    exit
fi

## Prep Docker Image 
# PUSHER_DOCKER_NAME="rchain-pusher-$(mktemp | awk -F. '{print $2}')"
# Use the above vs below if you want unique build names every time. 
# If build fails before last line with "rm -f" the container will stay running for more exploration. 
PUSHER_DOCKER_NAME="rchain-pusher-tmp"

# If container exists force remove it.
if [[ $(docker ps -aq -f name=${PUSHER_DOCKER_NAME}) ]]; then
    docker rm -f ${PUSHER_DOCKER_NAME}
fi

if [[ "${TRAVIS}" = "true" ]]; then
    # Start docker container with access to docker.sock so it can create and push docker images.
    docker run -dit -v /var/run/docker.sock:/var/run/docker.sock \
        -e DOCKER_USERNAME="${DOCKER_USERNAME}" \
        -e DOCKER_PASSWORD="${DOCKER_PASSWORD}" \
        -e TRAVIS="${TRAVIS}" -e TRAVIS_BRANCH=${TRAVIS_BRANCH} \
        --name ${PUSHER_DOCKER_NAME} ubuntu:16.04

    echo "Running Travis build and will push to Docker Hub repo deppending on branch name."

    # Copy and run build and push docker script in docker pusher container from above.
    docker cp rchain-docker-build-push.sh ${PUSHER_DOCKER_NAME}:/ 
    docker exec -it ${PUSHER_DOCKER_NAME} bash -c "./rchain-docker-build-push.sh \
        ${branch_name} ${git_repo} ${docker_dst_repo}"
else
    echo "Running local build and push to Docker repo."
    docker run -dit -v /var/run/docker.sock:/var/run/docker.sock \
        --name ${PUSHER_DOCKER_NAME} ubuntu:16.04
    # Copy and run build and push docker script in docker pusher container from above.
    docker cp rchain-docker-build-push.sh ${PUSHER_DOCKER_NAME}:/ 
    docker exec -it ${PUSHER_DOCKER_NAME} bash -c "./rchain-docker-build-push.sh \
        ${branch_name} ${git_repo} ${docker_dst_repo}"
fi

# Clean Up
docker rm -f ${PUSHER_DOCKER_NAME}
