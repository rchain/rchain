RHOLANG_JAR	= ../rholang/target/scala-2.12/rholang-assembly-0.1.0-SNAPSHOT.jar
RHOLANG_BNFC    = ../rholang/src/main/java/coop/
ROSETTE_BIN	= ../rosette/build.out/src/rosette
RBL_DIR		= ../rosette/rbl/rosette

rholang: $(RHOLANG_JAR)

$(RHOLANG_JAR): $(RHOLANG_BNFC)
	@cd .. && sbt "rholang / assembly"

$(RHOLANG_BNFC):
	@cd .. && sbt "rholang / bnfc:generate"

rosette: $(ROSETTE_BIN)

$(ROSETTE_BIN):
	@cd ../rosette && ./build.sh

rholang-cli: rholang rosette
	@docker build -f $@/Dockerfile .. -t $@

.PHONY: rholang rosette rholang-web rholang-cli
