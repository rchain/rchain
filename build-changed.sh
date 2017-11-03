#!/bin/bash
 
COUNT=0
 
#list subdirectories
for d in */ ; do
    #check for sbt subproject
    if [ -f "$d/build.sbt" ]
    then
        #check for changes
        if git diff --name-only HEAD^ | grep "$d" > /dev/null
        then
            DIFF[COUNT]="$d"
            COUNT=$((COUNT+1))
        fi
    fi
done
 
#run test for all subprojects with changes
for i in ${DIFF[@]}; do
    echo "Run sbt"
    cd "$i"
    sbt test
    cd ../
done
