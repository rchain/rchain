#!/usr/bin/env bash
## Set BASH environment so it will fail properly throwing exit code
set -exo pipefail

## Prep docker image - note to use docker within docker we need to mount /var/run/docker.sock to our docker build image. 
# pusher_docker_name="rchain-pusher-$(mktemp | awk -F. '{print $2}')"
# Use the above vs below if you want unique build names every time. 
# If build fails before last line with "rm -f" this can leave garbage containers running. 
pusher_docker_name="rchain-pusher-tmp"
docker rm -f ${pusher_docker_name} 

# Start docker container with access to docker.sock so it can run view/run docker images
docker run -dit -v /var/run/docker.sock:/var/run/docker.sock \
    -e DOCKER_USERNAME="${DOCKER_USERNAME}" \
    -e DOCKER_PASSWORD="${DOCKER_PASSWORD}" \
    -e TRAVIS="${TRAVIS}" -e TRAVIS_BRANCH=${TRAVIS_BRANCH} \
    --name ${pusher_docker_name} ubuntu:16.04
# Be aware of what "-v /var/run/docker.sock:/var/run/docker.sock" is doing above.
# See https://jpetazzo.github.io/2015/09/03/do-not-use-docker-in-docker-for-ci/

# Copy and run build and push docker script in docker pusher container from above
docker cp rchain-docker-build-push.sh ${pusher_docker_name}:/ 
if [[ "${TRAVIS_BRANCH}" = "master" || "${TRAVIS_BRANCH}" = "dev" ]] ; then
docker exec -it ${pusher_docker_name} bash -c "./rchain-docker-build-push.sh dev https://github.com/rchain/rchain rchain/rnode:${TRAVIS_BRANCH}"
elif [[ "${TRAVIS}" = "true" ]]; then
    echo "Currently only Travis branches master and dev pushed to docker hub."
    echo "Your branch ${TRAVIS_BRANCH} will not be pushed"
else
    echo "Uncomment and modify docker repo in $1 if you want to push to custom repo"
    # docker exec -it ${pusher_docker_name} bash -c "./rchain-docker-build-push.sh dev https://github.com/rchain/rchain rchain/rnode:mytagname"
    docker exec -it ${pusher_docker_name} bash -c "./rchain-docker-build-push.sh dev https://github.com/rchain/rchain"
fi

# Clean up
docker rm -f ${pusher_docker_name}
