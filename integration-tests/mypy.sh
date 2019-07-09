#!/bin/bash -ue

main () {
    pushd "$(dirname $0)" >/dev/null
    env PYENV_VERSION=3.7.3 ~/.pyenv/shims/python -m pipenv run mypy "$@" test
}

main "$@"
