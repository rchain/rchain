#!/usr/bin/env bash
## Tag and push newly built image if correct repo and branch.
set -exo pipefail

# ${DOCKER_DST_REPO}
DOCKER_DST_REPO="rchain/rnode"
DOCKER_DST_TAG="${TRAVIS_BRANCH}"

# This is repo:tag created by sbt command below 
DOCKER_SRC_REPO="coop.rchain/rnode"
DOCKER_SRC_TAG="latest"

echo "TRAVIS_BRANCH = ${TRAVIS_BRANCH}"
echo "TRAVIS_PULL_REQUEST = ${TRAVIS_PULL_REQUEST}"
echo "TRAVIS_REPO_SLUG = ${TRAVIS_REPO_SLUG}"

# Tag and push rnode docker container when it meets criteria.
if [[ "${TRAVIS_BRANCH}" = "master" || \
      "${TRAVIS_BRANCH}" = "dev" || \
      "${TRAVIS_BRANCH}" = "ops-test" ]] \
&& [[ "${TRAVIS_PULL_REQUEST}" = "false" && "${TRAVIS_REPO_SLUG}" = "rchain/rchain" ]] ; then

    # Generate RChain "RNode" network node docker container
    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker:publishLocal

    # Note: Secret Travis environmental variables are not available on pull requests as a means of protection.
    # Hence, the TRAVIS_PULL_REQUEST check and only commit on rchain/rchain repo.
    # ref https://docs.travis-ci.com/user/pull-requests/#Pull-Requests-and-Security-Restrictions

    echo "Travis branch ${TRAVIS_BRANCH} matched and from repo rchain/rchain. Pushing rnode to Docker repo."
    docker login -u "${DOCKER_USERNAME}" -p "${DOCKER_PASSWORD}" 
    docker tag  ${DOCKER_SRC_REPO}:${DOCKER_SRC_TAG} ${DOCKER_DST_REPO}:${DOCKER_DST_TAG} 
    docker push ${DOCKER_DST_REPO}:${DOCKER_DST_TAG} 
else
    echo "Container image not pushed as it is not correct branch and from rchain/rchain repo."
fi
