# -*- coding: utf-8; mode: dockerfile -*-
#
# Copyright © 2018 RChain Cooperative
# Author: Chris Kirkwood-Watts <kirkwood@pyrofex.net>
#
# This file is licensed under the Apache License, version 2.0.

FROM ubuntu:16.04

RUN dpkg --add-architecture i386
RUN apt-get update && apt-get install -y openjdk-8-jre-headless libstdc++6:i386

ENV ESS_SYSDIR /usr/local/lib/rosette
ENV ROSETTE_LIB /usr/local/lib/rosette

ENV RHOLANG_JAR /usr/local/lib/rholang-assembly-0.1.0-SNAPSHOT.jar
COPY rholang/target/scala-2.12/rholang-assembly-0.1.0-SNAPSHOT.jar /usr/local/lib

COPY rosette/rbl/rosette /usr/local/lib/rosette
COPY rosette/build.out/src/rosette /usr/local/bin/rosette

COPY docker/rholang-cli/run-rhoscala /usr/local/bin

ENTRYPOINT [ "/usr/local/bin/run-rhoscala" ]

# Local Variables:
# indent-tabs-mode: nil
# fill-column: 79
# comment-column: 37
# End:
