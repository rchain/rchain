# $ nix-env -q protobuf
# protobuf-3.5.1.1
PROTOC=protoc

# protoc-gen-doc
# c35c0525c468 Aug 27, 2018
PGENDOC=$(HOME)/golang/src/protoc-gen-doc/protoc-gen-doc

MODELS=../../models
PROTOS=$(MODELS)/src/main/protobuf
EXTERNAL=$(MODELS)/target/protobuf_external

index.md: $(PROTOS)/*.proto
	$(PROTOC) -I$(PROTOS) -I$(EXTERNAL) \
		--plugin=$(PGENDOC) --doc_out=. \
		--doc_opt=markdown,index.md:scalapb/* \
		$(PROTOS)/*.proto

index.html: $(PROTOS)/*.proto
	$(PROTOC) -I$(PROTOS) -I$(EXTERNAL) \
		--plugin=$(PGENDOC) --doc_out=. \
		--doc_opt=html,index.html:scalapb/* \
		$(PROTOS)/*.proto

clean:
	rm -f grpc.md index.html
