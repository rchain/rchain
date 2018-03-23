# do rholang tests
# Set bash output verbose and fail hard
#set -euxo pipefail
# dirname $(readlink -f a.sh)
set -exo pipefail
if [ "$TRAVIS" = "true" ]; then 
#if [ -z ${TRAVIS+x} ]; then 
#if [ "$TRAVIS_BRANCH" = "master" ]; then 
    project_root_dir=${TRAVIS_BUILD_DIR}
else
    #project_root_dir=$(dirname $(pwd))
    project_root_dir=$(pwd)
fi

## Detect if running in docker container - setup using sudo accordingly
if [[ $(cat /proc/self/cgroup  | grep docker) = *docker* ]]; then
    echo "Running in docker container!"
    sudo=""
else
    sudo="sudo"
fi

rosette_root_dir=${project_root_dir}/rosette
rholang_root_dir=${project_root_dir}/rholang

cd ${project_root_dir}

