# syntax=docker/dockerfile:1

# --- Build stage ---------------------------------------------------------------
FROM rust:1.95.0 AS builder

# `protoc` (prost-build .proto codegen) + the protobuf well-known types
# (`google/protobuf/empty.proto` etc., shipped by libprotobuf-dev) + build-essential
# + perl (ring/lmdb-rkv C builds, already in the full rust image).
RUN apt-get update \
    && apt-get install -y --no-install-recommends protobuf-compiler libprotobuf-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . .

RUN cargo build --release -p rchain-node --bin rnode

# --- Runtime stage -------------------------------------------------------------
# Must match the builder's glibc (rust:1.95.0 = Debian trixie / glibc 2.41).
FROM debian:trixie-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/target/release/rnode /usr/local/bin/rnode

# `--profile docker` points `storage.data-dir` at /var/lib/rnode.
RUN useradd --create-home rnode \
    && mkdir -p /var/lib/rnode \
    && chown -R rnode:rnode /var/lib/rnode

USER rnode
WORKDIR /var/lib/rnode

ENTRYPOINT ["rnode"]
