#!/bin/bash

export TERM=dumb

#list subdirectories
for d in */ ; do
    #check for bash test script
    if [ -f "$d/test.sh" ]
    then
        echo "$d/test.sh"
        (cd $d; bash test.sh)

    #check for sbt subproject
    elif [ -f "$d/build.sbt" ]
    then
        echo "$d/build.sbt"
        (cd $d; sbt -Dsbt.log.noformat=true test)
    else
        echo SKIP: $d
    fi
done
