#!/usr/bin/env bash
# Prep "set" for CI if CI environment variable is set
if [[ "${CI}" = "true" ]]; then
    set -exo pipefail
else
    set
fi

if [[ "$#" != "3" ]]; then
    echo "Invalid amount of parameters."
    echo "Usage: $0 <branch name> <repo url> <docker hub repo>"
    echo "Usage: $0 mybranch https://github.com/myrepo/rchain myrepo/rchain:latest"
    exit
fi

# Set working branch
branch_name=$1
git_repo=$2
docker_dst_repo=$3

echo "Branch Name: $branch_name"
echo "Git Repo: $git_repo"
echo "Docker Repo: $docker_dst_repo"
echo "5 seconds to cancel if this information is not correct."
sleep 5 

# Install Docker-CE
apt update
apt-get install -y apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg |  apt-key add -
add-apt-repository    "deb [arch=amd64] https://download.docker.com/linux/ubuntu    xenial    stable"
apt update
apt install -y docker-ce sudo

# Get RChain Repo
apt -y install git
git clone ${git_repo} 
cd rchain
PROJECT_ROOT_DIR=$(pwd -P)
git checkout ${branch_name} 

### Install all dependencies on Ubuntu 16.04 LTS (Xenial Xerus) for RChain dev environment.

## Verify operating system (OS) version is Ubuntu 16.04 LTS (Xenial Xerus)
# Add more OS versions as necessary. 
version=$(cat /etc/*release | grep "^VERSION_ID" | awk -F= '{print $2}' | sed 's/"//g')
if [[ "$version" == "16.04" ]]; then
    echo "Running install on Ubuntu 16.04" 
else
    echo "Error: Not running on Ubuntu 16.04"
    echo "Exiting"
    exit
fi

## Resynchronize the package index files from their sources
apt-get update -yqq
## Install g++ multilib for cross-compiling as rosette is currently only 32-bit
apt-get install g++-multilib -yqq
## Install misc tools 
apt-get install cmake curl git -yqq
## Install Java OpenJDK 8
apt-get update -yqq
#  apt-get install default-jdk -yqq # alternate jdk install 
apt-get install openjdk-8-jdk -yqq

## Build Needed Crypto
# Build secp 
apt-get install autoconf libtool -yqq
cd ${PROJECT_ROOT_DIR}
./scripts/install_secp.sh

# Build libsodium
cd ${PROJECT_ROOT_DIR}
./scripts/install_sodium.sh

## Install Haskell Platform
# ref: https://www.haskell.org/platform/#linux-ubuntu
# ref: https://www.haskell.org/platform/ # all platforms
cd ${PROJECT_ROOT_DIR}
apt-get install haskell-platform -yqq

## Install BNFC Converter 
# ref: http://bnfc.digitalgrammars.com/
./scripts/install_bnfc.sh

## Install SBT 
cd ${PROJECT_ROOT_DIR}
apt-get install apt-transport-https -yqq
echo "deb https://dl.bintray.com/sbt/debian /" |  tee -a /etc/apt/sources.list.d/sbt.list
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
apt-get update -yqq
apt-get install sbt -yqq
## Install JFlex 
apt-get install jflex -yqq

## Build RChain via SBT build.sbt 
cd ${PROJECT_ROOT_DIR}
sbt bnfc:generate node/docker:publishLocal

## Tag and push newly built docker image(s).
# Setup auth, source image(s) and target/destination image(s) name in variables 
DOCKER_SRC_REPO="coop.rchain/rnode"
DOCKER_SRC_TAG="latest"
echo "Manual docker login to push to Docker repo:"
docker login
docker tag  ${DOCKER_SRC_REPO}:${DOCKER_SRC_TAG} ${docker_dst_repo}
docker push ${docker_dst_repo}
echo "Script is complete!"
