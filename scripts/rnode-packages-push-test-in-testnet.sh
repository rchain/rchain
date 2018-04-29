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
    eval "$(ssh-agent -s)" # Start ssh-agent cache
    ssh-add - <<< $(echo ${SSH_PRIVATE_KEY} | base64 --decode --ignore-garbage)

    # Generate RChain "RNode" network node debian and rpm packages - push to repo
    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/rpm:packageBin node/debian:packageBin 
    scp -P 10003 rnode/target/rnode_0.2.1_all.deb ${SSH_USERNAME}@repo.rchain.space/usr/share/nginx/html/rnode_${TRAVIS_BRANCH}_all.deb
    scp -P 10003 node/target/rpm/RPMS/noarch/rnode-0.2.1-1.noarch.rpm ${SSH_USERNAME}@repo.rchain.space/usr/share/nginx/html/rnode-${TRAVIS_BRANCH}.noarch.rpm

    # Update rnode test network containers with branch
    for i in {1..4}; do

        # If first node set rnode cmd so it acts as bootstrap server
        if [[ $i == 1 ]]; then
            rnode_cmd="rnode --port 30304 --standalone --name 0f365f1016a54747b384b386b8e85352 > /var/log/rnode.log 2>&1 &"
        else
            rnode_cmd="rnode --bootstrap rnode://0f365f1016a54747b384b386b8e85352@10.1.1.2:30304 > /var/log/rnode.log 2>&1 &"
        fi

        ssh -p 4000{$i} ${SSH_USERNAME}@repo.rchain.space " 
            rm rnode_${TRAVIS_BRANCH}_all.deb;
            wget https://repo.rchain.space/rnode_${TRAVIS_BRANCH}_all.deb;
            pkill rnode;
            pkill java;
            apt -y --purge rnode;
            apt -y install ./rnode_${TRAVIS_BRANCH}_all.deb;
            ${rnode_cmd};
            exit
            " 
    done

else
    echo "Ignored. Build and tests not ran as not correct branch and from rchain/rchain repo."
fi
