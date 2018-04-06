#!/usr/bin/env bash
## Set BASH environment if on CI so it will fail properly throwing exit code
if [[ "${CI}" = "true" ]]; then
    set -exo pipefail
fi

if [[ "$#" != "0" && "$#" != "3" ]]; then
    echo "Invalid number of parameters."
    echo "Example: $0 dev https://github.com/rchain/rchain jeremybusk/rnode:mytagname"
    echo "You will be asked for you docker repo password." 
    echo "Type Docker repo user/pass in correctly the first time or script will fail."
    exit
fi

## Prep docker image 
# PUSHER_DOCKER_NAME="rchain-pusher-$(mktemp | awk -F. '{print $2}')"
# Use the above vs below if you want unique build names every time. 
# If build fails before last line with "rm -f" this can leave garbage containers running. 
PUSHER_DOCKER_NAME="rchain-pusher-tmp"

# If container exists force remove it
if [[ $(docker ps -aq -f name=${PUSHER_DOCKER_NAME}) ]]; then
    docker rm -f ${PUSHER_DOCKER_NAME}
fi

# Start docker container with access to docker.sock so it can run view/run docker images
docker run -dit -v /var/run/docker.sock:/var/run/docker.sock \
    -e DOCKER_USERNAME="${DOCKER_USERNAME}" \
    -e DOCKER_PASSWORD="${DOCKER_PASSWORD}" \
    -e TRAVIS="${TRAVIS}" -e TRAVIS_BRANCH=${TRAVIS_BRANCH} \
    --name ${PUSHER_DOCKER_NAME} ubuntu:16.04
# Be aware of what "-v /var/run/docker.sock:/var/run/docker.sock" is doing above.
# See https://jpetazzo.github.io/2015/09/03/do-not-use-docker-in-docker-for-ci/

# Copy and run build and push docker script in docker pusher container from above
docker cp rchain-docker-build-push.sh ${PUSHER_DOCKER_NAME}:/ 
if [[ "${TRAVIS}" = "true" ]]; then
    echo "Running Travis build and will push to Docker Hub repo deppending on branch name."
    docker exec -it ${PUSHER_DOCKER_NAME} bash -c "./rchain-docker-build-push.sh \
        dev \
        https://github.com/rchain/rchain \
        rchain/rnode:${TRAVIS_BRANCH}"
    echo "Currently the only Travis branches master and dev pushed to docker hub."
    echo "Your branch ${TRAVIS_BRANCH} will not be pushed"
elif [[ $1 && $2 && $3 ]]; then
    echo "Running custom build"
    custom_branch_name=$1
    custom_git_repo=$2
    custom_docker_dst_repo="$3"
    docker exec -it ${PUSHER_DOCKER_NAME} bash -c "./rchain-docker-build-push.sh \
        ${custom_branch_name} ${custom_git_repo} ${custom_docker_dst_repo}"
else
    echo "Docker build and push not supported. You must pass branch git-repo and dst docker repo"
    echo "Example: $0 dev https://github.com/rchain/rchain jeremybusk/rnode:mytagname"
fi

# Clean up
docker rm -f ${PUSHER_DOCKER_NAME}
