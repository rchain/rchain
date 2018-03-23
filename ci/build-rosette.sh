#!/usr/bin/env bash
source ci/header.sh

sudo apt install g++ g++-multilib cmake
cd "${rosette_root_dir}"; ./build.sh
cd ${project_root_dir}

