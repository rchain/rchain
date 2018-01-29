# -*- coding: utf-8; mode: dockerfile -*-
#
# Copyright © 2018 RChain Cooperative
# Author: Chris Kirkwood-Watts <kirkwood@pyrofex.net>
#
# This file is licensed under the Apache License, version 2.0.

FROM ubuntu:16.04

RUN dpkg --add-architecture i386
RUN apt-get update && apt-get install -y python3-pip openjdk-8-jre-headless libstdc++6:i386
RUN pip3 install --upgrade pip

RUN mkdir -p /opt/rosette/bin
RUN mkdir -p /opt/rosette/lib
COPY rosette/build.out/src/rosette /opt/rosette/bin
COPY rosette/rbl/rosette /opt/rosette/lib/rosette
ENV ROSETTE_LIB /opt/rosette/lib/rosette
ENV ESS_SYSDIR /opt/rosette/lib/rosette # muffle warning
ENV RHOLANGWEB_VM_PROGRAM /opt/rosette/bin/rosette
ENV RHOLANGWEB_VM_LIBRARY /opt/rosette/lib/rosette

RUN mkdir -p /opt/rholang/lib
COPY rholang/target/scala-2.12/rholang-assembly-0.1-SNAPSHOT.jar /opt/rholang/lib
ENV RHOLANGWEB_COMPILER_JAR /opt/rholang/lib/rholang-assembly-0.1-SNAPSHOT.jar

COPY rholang/examples /opt/rholang/lib/examples
ENV RHOLANGWEB_EXAMPLES_DIR /opt/rholang/lib/examples

COPY rholangweb /opt/rholangweb
WORKDIR /opt/rholangweb
RUN pip3 install -r requirements.txt
RUN python3 manage.py migrate

COPY docker/rholang-web/run-server .

ENTRYPOINT [ "/opt/rholangweb/run-server" ]

# Local Variables:
# indent-tabs-mode: nil
# fill-column: 79
# comment-column: 37
# End:
