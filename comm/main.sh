#!/usr/bin/env sh

java -Dfile.encoding=UTF8 -Djava.net.preferIPv4Stack=true -jar $RCHAIN_TARGET_JAR "$@"
