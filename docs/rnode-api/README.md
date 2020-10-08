# RNode API
[Use it live!](https://developer.rchain.coop/rnode-api)

`rnode` communicates primarily using gRPC but
it also has a Web API that supports use directly from browsers, `curl`, etc.

## extracting docs from gRPC .proto files

`index.md` is derived from `.proto` files. Details are in `Makefile`.
In particular:

```
MODELS=../../models
DEPLOY_PROTOS=$(MODELS)/src/main/protobuf
DIAG_PROTOS=../../node/src/main/protobuf

index.md: $(DEPLOY_PROTOS)/CasperMessage.proto $(DEPLOY_PROTOS)/RhoTypes.proto ...
```

## Swagger / OpenAPI documentation

`rnode-openapi.json` is a _best effort_ at an OpenAPI 2.0 (swagger) description
of the Web API for use in tools such as [Swagger UI](https://petstore.swagger.io/).

### NOTICE: OpenAPI docs may be out of sync

`rnode-openapi.json` was tested somewhat during initial development, but
unlike `integration-tests/test/test_web_api.py`, it is not
tested during continuous integration.

Much of it was generated directly from scala code that implements the API
but that effort is incomplete and considerable manual changes were made,
both to fix correctness bugs and to provide editorial material.

The `Makefile` includes a stanza for using `swagger-to-ts` to generate TypeScript.
The results are not currently mechanically tested but they are useful as a guide.
