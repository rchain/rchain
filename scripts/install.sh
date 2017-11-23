#!/bin/bash

if [ -f "$SUBPROJECT/install.sh" ]
then
    (cd $SUBPROJECT; ./install.sh)
fi
