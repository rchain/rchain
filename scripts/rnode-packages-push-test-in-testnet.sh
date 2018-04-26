#!/usr/bin/env bash
## Tag and push newly built image if correct repo and branch.
set -exo pipefail

echo "TRAVIS_BRANCH = ${TRAVIS_BRANCH}"
echo "TRAVIS_PULL_REQUEST = ${TRAVIS_PULL_REQUEST}"
echo "TRAVIS_REPO_SLUG = ${TRAVIS_REPO_SLUG}"

# Tag and push rnode docker container when it meets criteria.
if [[ "${TRAVIS_BRANCH}" = "master" || \
      "${TRAVIS_BRANCH}" = "dev" || \
      "${TRAVIS_BRANCH}" = "ops-test" ]] \
&& [[ "${TRAVIS_PULL_REQUEST}" = "false" && "${TRAVIS_REPO_SLUG}" = "rchain/rchain" ]] ; then # alternate if

    echo "Travis branch ${TRAVIS_BRANCH} matched and from repo rchain/rchain. Pushing rnode to Docker repo."

	# Prep ssh private key for use
    #mkdir ~/.travis
    echo "List .travis folder"
    ls -lhat ~/.travis/*
    echo "${SSH_PRIVATE_KEY}" | tr -d '\r' > ~/.travis/id_rsa
	eval "$(ssh-agent -s)" # Start ssh-agent cache
	chmod 600 ~/.travis/id_rsa # Allow read access to the private key
	ssh-add ~/.travis/id_rsa # Add the private key to SSH

    # Generate RChain "RNode" network node debian and rpm packages 
    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/rpm:packageBin node/debian:packageBin 
    scp -P 10003 rnode/target/rnode_0.2.1_all.deb ${SSH_USERNAME}@repo.rchain.space/usr/share/nginx/html/rnode_${TRAVIS_BRANCH}_all.deb
    scp -P 10003 node/target/rpm/RPMS/noarch/rnode-0.2.1-1.noarch.rpm ${SSH_USERNAME}@repo.rchain.space/usr/share/nginx/html/rnode-${TRAVIS_BRANCH}.noarch.rpm

    ssh -p 40004 << EOM
wget https://repo.rchain.space/rnode_${TRAVIS_BRANCH}_all.deb
apt --purge rnode
apt -y install ./rnode_${TRAVIS_BRANCH}_all.deb
EOM

else
    echo "Ignored. Build and tests not ran as not correct branch and from rchain/rchain repo."
fi
