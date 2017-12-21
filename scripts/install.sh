#!/bin/bash

if [ -f "$SUBPROJECT/install.sh" ]
then
    (cd $SUBPROJECT; ./install.sh)
elif [ -f "$SUBPROJECT/publish-docker-image.sh" ]
then
    (cd $SUBPROJECT; ./publish-docker-image.sh)
fi
