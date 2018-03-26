#!/usr/bin/env bash
export PATH=$PATH:$(pwd -P)/ci
source header.sh

sudo apt install g++ g++-multilib cmake
cd "${ROSETTE_ROOT_DIR}"; ./build.sh
cd ${PROJECT_ROOT_DIR}
