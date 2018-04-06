#!/usr/bin/env bash
if [[ "${CI}" = "true" ]]; then
    set -exo pipefail
fi

if [[ "$#" != "0" && "$#" != "3" ]]; then
    echo "Invalid amount of parameters."
    echo "Example: $0 <branch name> <repo url> <docker hub repo>"
    echo "Example: $0 mybranch https://github.com/myrepo/rchain myrepo/rchain:latest"
    exit
fi

# Set working branch
branch_name=$1
git_repo=$2
docker_dst_repo=$3

echo "Branch Name: $branch_name"
echo "Git Repo: $git_repo"
echo "Docker Repo: $docker_dst_repo"
echo "Travis Branch: ${TRAVIS_BRANCH}"
echo "Travis Docker Username: ${DOCKER_USERNAME}"
echo "5 seconds to cancel if this information is not correct."
sleep 5 


# Install Docker-CE
apt update
apt-get install -y apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg |  apt-key add -
add-apt-repository    "deb [arch=amd64] https://download.docker.com/linux/ubuntu    xenial    stable"
apt update
apt install -y docker-ce

# Get RChain Repo
apt -y install git
git clone ${git_repo} 
cd rchain
PROJECT_ROOT_DIR=$(pwd -P)
git checkout ${branch_name} 

### Install all dependencies on Ubuntu 16.04 LTS (Xenial Xerus) for RChain dev environment
## Detect if running in docker container - setup using sudo accordingly
if [[ $(cat /proc/self/cgroup  | grep docker) = *docker* ]]; then
    echo "Running in docker container!"
    sudo=""
else
    sudo="sudo"
fi

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
cd crypto
if [[ -d "secp256k1" ]]; then
    rm -rf secp256k1 
fi
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
./autogen.sh
./configure --enable-jni --enable-experimental --enable-module-schnorr --enable-module-ecdh --prefix=/tmp/f/rchain/.tmp
make

# Build libsodium
cd ${PROJECT_ROOT_DIR}
cd crypto
if [[ -d "libsodium" ]]; then
    rm -rf libsodium 
fi
git clone https://github.com/jedisct1/libsodium --branch stable
cd libsodium
./configure
make && make check
make install

## Install Haskell Platform
# ref: https://www.haskell.org/platform/#linux-ubuntu
# ref: https://www.haskell.org/platform/ # all platforms
cd ${PROJECT_ROOT_DIR}
apt-get install haskell-platform -yqq

## Install BNFC Converter 
# ref: http://bnfc.digitalgrammars.com/
bnfc_tmp_dir="$(mktemp -dt bnfcbuild.XXXXXX)"
cd ${bnfc_tmp_dir}
git clone https://github.com/BNFC/bnfc.git
cd bnfc/source
${sudo} cabal install --global
cd ${project_root}

## Install SBT 
cd ${PROJECT_ROOT_DIR}
apt-get install apt-transport-https -yqq
echo "deb https://dl.bintray.com/sbt/debian /" |  tee -a /etc/apt/sources.list.d/sbt.list
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
apt-get update -yqq
apt-get install sbt -yqq
## Install JFlex 
apt-get install jflex -yqq
## Remove temporary files 
rm -rf 

## Build RChain via SBT build.sbt 
cd ${PROJECT_ROOT_DIR}
sbt rholang/bnfc:generate
sbt rholang/compile
sbt rholang/assembly
sbt rspace/compile
sbt rspace/assembly
sbt node/compile
sbt node/assembly
sbt node/docker

## Tag and push newly built docker image(s).
# Setup auth, source image(s) and target/destination image(s) name in variables 
DOCKER_SRC_REPO="coop.rchain/rnode"
DOCKER_SRC_TAG="latest"
if [[ ${docker_dst_repo} ]]; then
    if [[ "${TRAVIS_BRANCH}" = "master" || \
        "${TRAVIS_BRANCH}" = "dev" || \
        "${TRAVIS_BRANCH}" = "OPS-117" || \
        "${TRAVIS_BRANCH}" = "ops-test" ]] ; then
        echo "${DOCKER_PASSWORD}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
        docker tag  ${DOCKER_SRC_REPO}:${DOCKER_SRC_TAG} ${docker_dst_repo}
        docker push ${docker_dst_repo}
    elif [[ "${TRAVIS}" != "true" ]]; then
        docker login
        docker tag  ${DOCKER_SRC_REPO}:${DOCKER_SRC_TAG} ${docker_dst_repo}
        docker push ${docker_dst_repo}
    else
        echo "Container image not pushed."
    fi
fi

echo """Login to docker hub manually and push docker image 
docker login -u <username>
<enter pass>
docker push ${docker_dst_repo}:<your specific tag>
"""
