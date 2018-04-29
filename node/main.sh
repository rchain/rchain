#!/usr/bin/env sh

mkdir /var/lib/rnode # required directory for rnode service
java -Dfile.encoding=UTF8 -Djava.net.preferIPv4Stack=true -jar $RCHAIN_TARGET_JAR "$@"
