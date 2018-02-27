#!/usr/bin/env bash
### Install all dependencies on Ubuntu 16.04 LTS (Xenial Xerus) for RChain dev environment


## Set BASH environment so it will fail properly throwing exit code
set -euxo pipefail

project_root=$(pwd)

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
${sudo} apt-get update -yqq

## Install g++ multilib for cross-compiling as rosette is currently only 32-bit 
${sudo} apt-get install g++-multilib -yqq

## Install misc tools 
${sudo} apt-get install cmake curl git -yqq

## Install Java OpenJDK 8
${sudo} apt-get update -yqq
# ${sudo} apt-get install default-jdk -yqq # alternate jdk install 
${sudo} apt-get install openjdk-8-jdk -yqq

## Install needed crypto
${sudo} apt-get install autoconf libtool -yqq
cd crypto
if [ -d "secp256k1" ]; then
    rm -rf secp256k1 
fi
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
./autogen.sh
./configure --enable-jni --enable-experimental --enable-module-schnorr --enable-module-ecdh --prefix=$PWD/.tmp
make install
cd ${project_root}

## Install Haskell Platform
# ref: https://www.haskell.org/platform/#linux-ubuntu
# ref: https://www.haskell.org/platform/ # all platforms
${sudo} apt-get install haskell-platform -yqq

## Install BNFC Converter 
# ref: http://bnfc.digitalgrammars.com/
bnfc_tmp_dir="$(mktemp -dt bnfcbuild.XXXXXX)"
cd ${bnfc_tmp_dir}
git clone https://github.com/BNFC/bnfc.git
cd bnfc/source
${sudo} cabal install --global
cd ${project_root}

## Install sbt
${sudo} apt-get install apt-transport-https -yqq
echo "deb https://dl.bintray.com/sbt/debian /" | ${sudo} tee -a /etc/apt/sources.list.d/sbt.list
${sudo} apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
${sudo} apt-get update -yqq
${sudo} apt-get install sbt -yqq

## Install JFlex 
${sudo} apt-get install jflex -yqq

## Remove temporary files 
rm -rf ${bnfc_tmp_dir}
