#!/bin/bash -e

tag=latest

export DEFAULT_IMAGE=rchain-integration-tests:$tag

sed "s/rnode:latest/rnode:$tag/" Dockerfile | docker build --quiet --tag "$DEFAULT_IMAGE" --file - .

if [[ $(uname -s) = Darwin ]]; then
    env PYENV_VERSION=3.7.3 TMPDIR=/tmp ~/.pyenv/shims/python -m pipenv run python generate_rnode_data.py
else
    env PYENV_VERSION=3.7.3 ~/.pyenv/shims/python -m pipenv run python generate_rnode_data.py
fi