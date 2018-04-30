#!/usr/bin/env bash
# With Docker CE installed, this will build a simple private RChain P2P test network.
# The test network contains a bootstrap server and two more peers connecting to P2P network via bootstrap.
# "local repo" as params builds from current repo you are in
# "delete testnet" removes all testnet resources 

set -eo pipefail

NETWORK_UID="1" # Unique identifier for network if you wanted to run multiple test networks

delete_test_network_resources() {
  if [[ ! $1 ]]; then
    echo "E: Requires network name as argument"
    exit
  fi
  # Remove docker containers related to a network 
  network_name=$1
  echo "Removing all resources for test network $network_name"
  sleep 2  
  for i in $(docker container ps --format {{.Names}} | grep \.${network_name}$); do
    echo "Force removing docker container $i"
    sudo docker rm -f $i
  done
  
  if [[ "$(sudo docker network list --format {{.Name}} | grep ^${network_name}$)" != "" ]]; then
    echo "Removing docker network ${network_name}"
    sudo docker network rm ${network_name}
  fi
}

if [[ $1 && $2 ]]; then
  git_repo=$1
  branch_name=$2
  echo "Creating docker rnode test-net for ${git_repo} ${branch_name}"
elif [[ "${TRAVIS}" == "true" ]]; then
  echo "Running in TRAVIS CI"
  branch_name="${TRAVIS_BRANCH}"
else
  echo "Usage: $0 <repo url> <branch name>"
  echo "Usage: $0 https://github.com/rchain/rchain dev"
  echo "Special Usage Commands:"
  echo "Usage: $0 local repo"
  echo "Usage: $0 delete testnet"
  exit
fi

network_name="testnet${NETWORK_UID}.rchain"
sudo echo "" # Ask for sudo early

if [[ "${git_repo}" == "delete" && "${branch_name}" == "testnet" ]]; then
  delete_test_network_resources "${network_name}"
  exit
fi

delete_test_network_resources "${network_name}"

echo "Creating RChain rnode docker image coop.rchain/rnode from git src via sbt"
if [[ "${TRAVIS}" == "true" ]]; then
  sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker
elif [[ "${git_repo}" == "local" && "${branch_name}" == "repo" ]]; then
  cd ..
  git_dir=$(dirname $(pwd))
  sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker
else
  git_dir=$(mktemp -d /tmp/rchain-git.XXXXXXXX)
  cd ${git_dir}
  git clone ${git_repo} 
  cd rchain
  git checkout ${branch_name}
  sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker
fi

echo "Creating docker test network"
sudo docker network create \
  --driver=bridge \
  --subnet=169.254.1.0/24 \
  --ip-range=169.254.1.0/24 \
  --gateway=169.254.1.1 \
  ${network_name}

echo "Creating docker test containers"
for i in {0..2}; do
  container_name="node${i}.${network_name}"
  echo $container_name

  if [[ $i == 0 ]]; then
    rnode_cmd="--port 30304 --standalone --name 0f365f1016a54747b384b386b8e85352"
  else
    rnode_cmd="--bootstrap rnode://0f365f1016a54747b384b386b8e85352@169.254.1.2:30304"
  fi
  sudo docker run -dit --name ${container_name} \
    --network=${network_name} \
    coop.rchain/rnode ${rnode_cmd}

  sudo docker exec ${container_name} sh -c "apk add curl"
done
echo "Script has completed but it might take a minute for start-up of network and metrics to be available.\n\n"

echo "#########################DOCKER NOTES##########################"
echo "==============================================================="
echo "To display standalone bootstrap server rnode log:"
echo "sudo docker logs --follow node0.${network_name}"
echo "==============================================================="
echo "To display node1 rnode log:"
echo "sudo docker logs --follow node1.${network_name}"
echo "==============================================================="
echo "To view rnode metrics of bootstrap container:"
echo "sudo docker exec node0.${network_name} sh -c \"curl 127.0.0.1:9095\""
echo "==============================================================="
echo "To enter your bootstrap/standalone docker container:"
echo "sudo docker exec -it node0.${network_name} /bin/sh"
echo "==============================================================="
echo "Other Commands:"
echo "sudo docker ps"
echo "sudo docker network ls"
echo "sudo docker container ls"
echo "sudo docker image ls"
echo "sudo docker stop node2.${network_name}"
echo "==============================================================="
