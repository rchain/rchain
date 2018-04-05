#!/usr/bin/env bash
## Set BASH environment so it will fail properly throwing exit code
#set -euxo pipefail
# set -v # optional set if you want it verbose but not to fail hard

## Prep docker image - note to use docker within docker we need to mount /var/run/docker.sock to our docker build image. 
# pusher_docker_name="rchain-pusher-$(mktemp | awk -F. '{print $2}')"
# Use the above vs below if you want unique build names every time. 
# If build fails before last line with "rm -f" this can leave garbage containers running. 
pusher_docker_name="rchain-pusher-tmp"
docker rm -f ${pusher_docker_name} 
# Be aware of the security risks while this is running. 
# See https://jpetazzo.github.io/2015/09/03/do-not-use-docker-in-docker-for-ci/
docker run -dit -v /var/run/docker.sock:/var/run/docker.sock --name ${pusher_docker_name} ubuntu:16.04
docker cp rchain-docker-build-push.sh ${pusher_docker_name}:/ 
docker exec -it ${pusher_docker_name} bash -c "./rchain-docker-build-push.sh dev https://github.com/rchain/rchain rchain/rnode:jtester"
docker rm -f ${pusher_docker_name}
