#!/usr/bin/env bash
DOCKER_USERNAME="yourusername"
DOCKER_PASSWORD="yourpassword"
image_id="imageid"
image_tag="rchain-node:dev"
repo="${DOCKER_USERNAME}/${image_tag}"
docker tag ${image_id} $repo
docker login -u ${DOCKER_USERNAME} -p ${DOCKER_PASSWORD}
docker push $repo 
