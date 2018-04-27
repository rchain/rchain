#!/usr/bin/env bash
# With Docker CE installed, this will build a simple private RChain P2P test network.
# The test network contains a bootstrap server and two peers connecting to P2P network via bootstrap.
set -eo pipefail

NETWORK_UID="1" # Unique identifier for network if you wanted to run multiple test networks

if [[ $1 && $2 ]]; then
  git_repo=$1
  branch_name=$2
  echo "Creating docker rnode test-net for ${git_repo} ${branch_name}"
else
  echo "Usage: $0 <repo url>> <branch name>"
  echo "Usage: $0 https://github.com/rchain/rchain dev"
  exit
fi

network_name="${NETWORK_UID}.rnode.test.net"

sudo echo "" # Ask for sudo early

# Create debian package from git repo via sbt
git_dir=$(mktemp -d /tmp/rchain-git.XXXXXXXX)
cd ${git_dir}
git clone ${git_repo} 
cd rchain
git checkout ${branch_name}
sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/debian:packageBin

# Create docker network if it doesn't exist
if [[ "$(sudo docker network list --format {{.Name}} | grep ^${network_name}$)" == "" ]]; then
  sudo docker network create \
    --driver=bridge \
    --subnet=10.1.1.0/24 \
    --ip-range=10.1.1.0/24 \
    --gateway=10.1.1.1 \
    ${network_name}
else
  echo "The network ${network_name} already exists."
fi

for i in {0..2}; do
  container_name="node${i}.${network_name}"
  echo $container_name

  # If container exists force remove it.
  if [[ $(sudo docker ps -aq -f name=${container_name}) ]]; then
    sudo docker rm -f ${container_name}
  fi

  sudo docker run -dit --name ${container_name} \
    -v $git_dir/rchain/node/target/rnode_0.2.1_all.deb:/rnode_0.2.1_all.deb \
    --network=${network_name} \
    openjdk

  # Ret rnode run command depending on node nubmer
  if [[ $i == 0 ]]; then
    rnode_cmd="rnode --port 30304 --standalone --name 0f365f1016a54747b384b386b8e85352 > /var/log/rnode.log 2>&1 &"
  else
    rnode_cmd="rnode --bootstrap rnode://0f365f1016a54747b384b386b8e85352@10.1.1.2:30304 > /var/log/rnode.log 2>&1 &"
  fi

  branch_name="0.2.1"
  
  sudo docker exec ${container_name} bash -c "
    apt -y update;
    apt -y iputils-ping bridge-utils iproute2;
    apt -y install ./rnode_${branch_name}_all.deb;
    ${rnode_cmd}
    " 
done

echo "#########################DOCKER NOTES##########################"
echo "==============================================================="
echo "To display standalone bootstrap server rnode log:"
echo "sudo docker exec node0.${network_name} bash -c \"tail -f /var/log/rnode.log\""
echo "==============================================================="
echo "To display node1 rnode log:"
echo "sudo docker exec  node1.1.rnode.test.net bash -c \"tail -f /var/log/rnode.log\""
echo "==============================================================="
echo "To go into your standalone docker container:"
echo "sudo docker exec -it node0.${network_name} /bin/bash"
echo "==============================================================="
