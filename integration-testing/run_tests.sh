#!/usr/bin/env bash

docker build .

echo ==========================================================================================================================
echo === Tests are slow and logging is displayed only for failing ones so it might look like there is no activity happening
echo === You can watch the docker containers for activity
echo === If you want live logs you can enable them by setting 'log_cli=true' in pytest.ini
echo ==========================================================================================================================
PYTHONPATH=src python -m pytest
