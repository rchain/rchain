#!/usr/bin/env bash

docker build .

echo ==========================================================================================================================
echo === Tests are slow and logging is displayed only for failing ones so it might look like there is no activity happening
echo === You can watch the log file, see pytest.ini:log_file, or you can watch the logs of docker containers for activity
echo === If you want live logs you can enable them by setting 'log_cli=true'
echo === for more command line options use \'$0 --help\'
echo ==========================================================================================================================

PYTHONPATH=src .virtualenv/bin/python -m pytest -v $@
