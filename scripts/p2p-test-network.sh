#!/usr/bin/env bash
# With Docker CE installed, this will build a simple private RChain P2P test network.
# The test network contains a bootstrap server and two more peers connecting to P2P network via bootstrap.
# "local repo" as params builds from current repo you are in
# "delete testnet" removes all testnet resources 

set -eo pipefail

NETWORK_UID="1" # Unique identifier for network if you wanted to run multiple test networks
network_name="testnet${NETWORK_UID}.rchain"


create_test_network_resources() {
  if [[ ! $1 ]]; then
    echo "E: Requires network name as argument"
    exit
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

  echo "Test network build has completed but it might take a minute for start-up of network and metrics to be available."

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
  echo "Other Useful Commands:"
  echo "sudo docker ps"
  echo "sudo docker network ls"
  echo "sudo docker container ls"
  echo "sudo docker image ls"
  echo "sudo docker stop node2.${network_name}"
  echo "==============================================================="
}


delete_test_network_resources() {
  if [[ ! $1 ]]; then
    echo "E: Requires network name as argument"
    exit
  fi
  # Remove docker containers related to a network 
  network_name=$1
  for i in $(docker container ls --all --format {{.Names}} | grep \.${network_name}$); do
    echo "Removing docker container $i"
    sudo docker container rm -f $i
  done
  
  if [[ "$(sudo docker network list --format {{.Name}} | grep ^${network_name}$)" != "" ]]; then
    echo "Removing docker network ${network_name}"
    sudo docker network rm ${network_name}
  fi
}


# Process params
if [[ "${TRAVIS}" == "true" ]]; then
  echo "Running in TRAVIS CI"
  git_repo="local"
  branch_name="${TRAVIS_BRANCH}"
elif [[ $1 == "local" ]]; then
  git_repo="local"
elif [[ $1 == "start" ]]; then
  delete_test_network_resources "${network_name}"
  create_test_network_resources "${network_name}"
  exit
elif [[ $1 == "stop" ]]; then
  delete_test_network_resources "${network_name}"
  exit
elif [[ $1 && $2 ]]; then
  git_repo=$1
  branch_name=$2
  echo "Creating docker rnode test-net for ${git_repo} ${branch_name}"
else
  echo "Usage: $0 <repo url> <branch name>"
  echo "Usage: $0 https://github.com/rchain/rchain dev"
  echo "Special Usage Commands:"
  echo "Usage: $0 local"
  echo "Usage: $0 start"
  echo "Usage: $0 stop"
  echo "Usage: $0 delete testnet"
  exit
fi

# Remove test network resources before creating new network
delete_test_network_resources "${network_name}"

sudo echo "" # Ask for sudo early
echo "Creating RChain rnode docker image coop.rchain/rnode from git src via sbt"
if [[ "${git_repo}" == "local" ]]; then
  sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker
else
  git_dir=$(mktemp -d /tmp/rchain-git.XXXXXXXX)
  cd ${git_dir}
  git clone ${git_repo} 
  cd rchain
  git checkout ${branch_name}
  sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker
fi

create_test_network_resources "${network_name}"
