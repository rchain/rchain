# $ nix-env -q protobuf
# protobuf-3.5.1.1
PROTOC=protoc

# protoc-gen-doc
# c35c0525c468 Aug 27, 2018
PGENDOC=$(HOME)/golang/src/protoc-gen-doc/protoc-gen-doc

MODELS=../../models
DEPLOY_PROTOS=$(MODELS)/src/main/protobuf
DIAG_PROTOS=../../node/src/main/protobuf

EXTERNAL=$(MODELS)/target/protobuf_external

rnode-openapi-schema.ts:
	npx @manifoldco/swagger-to-ts rnode-openapi.json --output rnode-openapi-schema.ts

index.md: $(DEPLOY_PROTOS)/CasperMessage.proto $(DEPLOY_PROTOS)/RhoTypes.proto \
		$(DIAG_PROTOS)/diagnostics.proto
	$(PROTOC) -I$(EXTERNAL) -I$(DIAG_PROTOS) -I$(DEPLOY_PROTOS) --plugin=$(PGENDOC) \
		--doc_out=. --doc_opt=markdown,index.md:scalapb/* \
		$(DEPLOY_PROTOS)/CasperMessage.proto $(DEPLOY_PROTOS)/RhoTypes.proto \
			$(DIAG_PROTOS)/diagnostics.proto


index.html: index.md
	markdown index.md >index.html

clean:
	rm -f index.md index.html
