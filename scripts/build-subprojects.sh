#!/bin/bash

if [ -f "build.sbt" ]; then
    case ${SUBPROJECT} in
	rholang)
	    sbt_commands=";project ${SUBPROJECT} ;bnfc:generate ;compile ;assembly"
	    ;;
	comm|storage)
	    sbt_commands=";project ${SUBPROJECT} ;compile ;assembly"
	    ;;
	node)
	    sbt_commands=";project rholang ;bnfc:generate ;assembly ;project ${SUBPROJECT} ;assembly ;docker"
	    ;;
    esac
    if [ -z "$sbt_commands" ]; then
	exit 0
    fi
    # Add coverage and any other common things here.
    # sbt_commands="$sbt_commands ..."
    sbt -Dsbt.log.noformat=true "$sbt_commands"
else
    echo "No build/test files found!"
    exit 1
fi
