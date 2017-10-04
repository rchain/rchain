
all: docker

docker:
	sbt package
	@cp target/scala-2.10/rchain-comm-doodles_2.10-0.1-SNAPSHOT.jar ./docker/jar-dependencies
	@(cd docker; docker build . -t commtest:latest)

.PHONY: docker
