#!/usr/bin/env bash
# Prep "set" for CI if CI environment variable is set
if [[ "${CI}" = "true" ]]; then
    set -exo pipefail
else
    set
fi

# Receive Params and Set Variables
if [[ $1 && $2 && $3 ]]; then
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

echo "Running local build and push to Docker repo."
docker run -dit -v /var/run/docker.sock:/var/run/docker.sock \
    --name ${PUSHER_DOCKER_NAME} ubuntu:16.04
# Copy and run build and push docker script in docker pusher container from above.
docker cp rnode-docker-build-push-script.sh ${PUSHER_DOCKER_NAME}:/ 
docker exec -it ${PUSHER_DOCKER_NAME} bash -c "./rnode-docker-build-push-script.sh \
    ${branch_name} ${git_repo} ${docker_dst_repo}"

# Clean Up
docker rm -f ${PUSHER_DOCKER_NAME}
