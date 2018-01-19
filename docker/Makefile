RHOLANG_JAR	= ../rholang/target/scala-2.12/rholang-assembly-0.1-SNAPSHOT.jar
ROSETTE_BIN	= ../rosette/build.sh/src/rosette
RBL_DIR		= ../rosette/rbl/rosette

rholang:
	@cd ../rholang && sbt bnfc:generate && sbt assembly

rosette:
	@cd ../rosette && ./build.sh

rholang-web: rholang rosette
	@docker build -f $@/Dockerfile .. -t $@

rholang-cli: rholang rosette
	@docker build -f $@/Dockerfile .. -t $@

.PHONY: rholang rosette rholang-web rholang-cli
