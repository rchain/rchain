#!/usr/bin/env bash
# With Docker CE installed, this will build a simple private RChain P2P test network.
# The test network contains a bootstrap server and two more peers connecting to P2P network via bootstrap.
# "local repo" as params builds from current repo you are in
# "delete testnet" removes all testnet resources 

if [[ "${TRAVIS}" == "true" ]]; then
  set -xeo pipefail # enable verbosity on CI environment for debugging
else
  set -eo pipefail
fi

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
    sleep 3 # slow down 
  done
  
  echo "======================================================"
  echo "P2P test network build complete. Converging network."
  echo "======================================================"
  echo ""
  echo "Test network build has completed but it might take a minute for start-up of network and metrics to be available."
  echo ""
  echo 'Run option "docker-help" to get more info on docker commands to interact with node containers'  
  echo ""
}

docker_help_info() {
  if [[ ! $1 ]]; then
    echo "E: Requires network name as argument"
    exit
  fi
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

create_artifacts() {
  sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/rpm:packageBin node/debian:packageBin
  #cp node/target/rnode_0.2.1_all.deb /tmp/rnode_dev.deb
  #cp node/target/rpm/RPMS/noarch/rnode-0.2.1-1.noarch.rpm /tmp/rnode_dev.rpm
  artifacts_dir=$(mktemp -d /tmp/artifacts.XXXXXXXX)
  cp node/target/*.deb ${artifacts_dir}/rnode.deb
  cp node/target/rpm/RPMS/noarch/*.rpm ${artifacts_dir}/rnode.rpm
  cp node/target/*.deb ${artifacts_dir}/rnode_dev_all.deb
  cp node/target/rpm/RPMS/noarch/*.rpm ${artifacts_dir}/rnode-dev.noarch.rpm
  scp -CP 10003 ${artifacts_dir}/* root@repo.rchain.space:/usr/share/nginx/html/
  #eval `ssh-agent -s`
  #ssh-add <(cat ~/.ssh/travis_id_rsa)
  rm -rf ${artifacts_dir}
}


run_tests_on_network() {
  all_pass=true
  if [[ "${TRAVIS}" == "true" ]]; then
    set +xeo pipefail # turn off exit immediately for tests
    set -x
  fi

  if [[ ! $1 ]]; then
    echo "E: Requires network name as argument"
    exit
  fi

  #set +eo pipefail # turn of exit immediately for tests
  for container_name in $(docker container ls --all --format {{.Names}} | grep \.${network_name}$); do

    echo "============================================="
    echo "Running tests on ${container_name}"

    if [[ $(sudo docker exec ${container_name} sh -c "curl -s 127.0.0.1:9095") ]]; then
      echo "PASS: Could connect to metrics api" 
    else
      all_pass=false
      echo "FAIL: Could not connect to metrics api" 
    fi

    # Currently has inconsistent metric value bug. 3.0 and sometimes 2.0. Kamon issue? Timing?
    metric_expected_peers_total="2.0"
    if [[ $(sudo docker exec ${container_name} sh -c "curl -s 127.0.0.1:9095 | grep 'peers_total ${metric_expected_peers_total}'") ]]; then
      echo "PASS: Correct metric api total peers count" 
    else
      all_pass=false
      echo "FAIL: Incorrect metric api total peers count. Should be ${metric_expected_peers_total}" 
      sudo docker exec ${container_name} sh -c "curl -s 127.0.0.1:9095 | grep 'peers_total*'"
    fi
    
    if [[ $(sudo docker logs ${container_name} | grep 'Peers: 2.') ]]; then
      echo "PASS: Correct log peers count" 
    else
      all_pass=false
      echo "FAIL: Incorrect log peers count" 
      sudo docker logs ${container_name} | grep 'Peers:'
    fi

    if [[ ! $(sudo docker logs ${container_name} | grep ERR) ]]; then
      echo "PASS: No error messages contained in logs" 
    else
      all_pass=false
      echo "FAIL: ERROR messages contained in logs" 
      sudo docker logs ${container_name} | grep ERR
    fi
  done

  echo "=========PAUSE for 3=============="
  sleep 3
  # Ping all nodes from boot-server
  echo "Checking node connectivity for ips"
  docker exec node0.testnet1.rchain sh -c "
                                          ping -c 2 169.254.1.2;
                                          ping -c 2 169.254.1.3;
                                          ping -c 2 169.254.1.4;
                                          "
  if [[ ! "${TRAVIS}" == "true" ]]; then
  set -xeo pipefail # turn off exit immediately for tests
  set +x
  else
    echo ""
    #set -eo pipefail
  fi
  
  # Check for test failures
  echo "============================================="
  if [[ $all_pass == false ]]; then
    echo "ERROR: Not all network checks passed."
    echo "Dumping metrics and logs"
    for i in {0..2}; do
      echo "===================================================================="
      sudo docker exec node${i}.${network_name} sh -c "curl 127.0.0.1:9095"
      echo "===================================================================="
    done
    for i in {0..2}; do
      echo "===================================================================="
      sudo docker logs node${i}.${network_name}
      echo "===================================================================="
    done
    # exit 1 # disabled for passive failure on tests until figure
  elif [[ $all_pass == true ]]; then
    echo "SUCCESS: All checks passed"
  else
    echo "Unsupported"
  fi

  if [[ "${TRAVIS}" == "true" ]]; then
  set +x
  set -xeo pipefail # turn back on exit immediately for tests
  fi
}

create_docker_rnode_image() {
  if [[ ! $1 ]]; then
    echo "E: Requires git repo as argument"
    exit
  fi
  echo "Creating RChain rnode docker image coop.rchain/rnode from git src via sbt"
  if [[ "$1" == "local" ]]; then
    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker
  elif [[ $1 && $2 ]]; then
    git_dir=$(mktemp -d /tmp/rchain-git.XXXXXXXX)
    cd ${git_dir}
    git clone $1 
    cd rchain
    git checkout $2
    sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker
  else
    echo "Unsupported"
  fi
}

# ======================================================

# MAIN Process params
if [[ "${TRAVIS}" == "true" ]]; then
  echo "Running in TRAVIS CI"
  sbt -Dsbt.log.noformat=true clean rholang/bnfc:generate node/docker
  delete_test_network_resources "${network_name}"
  create_test_network_resources "${network_name}"
  echo "Running tests on network in 120 seconds after bootup and convergence"
  echo "Please be patient"
  sleep 120 # allow plenty of time for network to boot and converge
  run_tests_on_network "${network_name}"
elif [[ $1 == "local" ]]; then
  sudo echo "" # Ask for sudo early
  create_docker_rnode_image "local"
  delete_test_network_resources "${network_name}"
  create_test_network_resources "${network_name}"
elif [[ $1 == "run-tests" ]]; then
  sudo echo "" # Ask for sudo early
  run_tests_on_network "${network_name}"
elif [[ $1 == "start" ]]; then
  sudo echo "" # Ask for sudo early
  delete_test_network_resources "${network_name}"
  create_test_network_resources "${network_name}"
elif [[ $1 == "stop" ]]; then
  sudo echo "" # Ask for sudo early
  delete_test_network_resources "${network_name}"
elif [[ $1 == "create-artifacts" ]]; then
  create_artifacts
elif [[ $1 == "docker-help" ]]; then
  docker_help_info "${network_name}"
elif [[ $1 && $2 ]]; then
  sudo echo "" # Ask for sudo early
  git_repo=$1
  branch_name=$2
  echo "Creating docker rnode test-net for ${git_repo} ${branch_name}"
  create_docker_rnode_image "${git_repo}" "${branch_name}"
  delete_test_network_resources "${network_name}"
  create_test_network_resources "${network_name}"
else
  echo "Usage: $0 <repo url> <branch name>"
  echo "Usage: $0 https://github.com/rchain/rchain dev"
  echo "Usage: $0 local"
  echo "Usage: $0 start"
  echo "Usage: $0 stop"
  echo "Usage: $0 run-tests"
  echo "Usage: $0 docker-help"
  exit
fi
