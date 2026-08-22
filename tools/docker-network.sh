#!/usr/bin/env bash
#
# Docker multi-node test network (1..5 nodes) for the Rust `rnode`.
#
#   tools/docker-network.sh build          build the rnode image
#   tools/docker-network.sh up [N]         start a bootstrap + N-1 peers (default 3)
#   tools/docker-network.sh down           stop the network (+ optional -v to drop volumes)
#   tools/docker-network.sh status         docker ps for the network
#   tools/docker-network.sh logs <node>    tail a node's logs
#   tools/docker-network.sh cli <node> <rnode subcommand...>
#
# Nodes serve Deploy on the external gRPC port 40401 and Propose+Repl on the loopback-only
# internal port 40402. `cli` runs the Rust `rnode` client *inside* the node container (via
# `docker exec`) so it can reach the loopback-bound propose/repl server; the host needs only
# docker + openssl.
#
# Prereqs: docker, openssl (to read the bootstrap node-id from its generated TLS cert).

set -euo pipefail

cd "$(dirname "$0")/.."

IMAGE="${RNODE_IMAGE:-rnode:local}"
NETWORK="rnode-net"
BOOTSTRAP="bootstrap"
PEER_PREFIX="peer"

# Fixed validator key (matches the node integration-test `VALIDATOR_PRIV_HEX`); the bootstrap is
# bonded with this key so it can create genesis and propose.
VALIDATOR_PRIV_HEX="a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76"
VALIDATOR_PUB_HEX="04f700a417754b775d95421973bdbdadb2d23c8a5af46f1829b1431f5c136e549e8a0d61aa0c793f1a614f8e437711c7758473c6ceb0859ac7e9e07911ca66b5c4"
STAKE=100

# Host port mapped to the bootstrap's deploy gRPC service (in-container 40401); peers get
# 40402 + 1000*i. Propose/repl bind loopback-only (127.0.0.1:40402) and are never host-mapped.
GRPC_BASE=40402

usage() {
  sed -n '2,15p' "$0" >&2
  exit 2
}

cmd_build() {
  docker build -f docker/rnode/Dockerfile -t "$IMAGE" .
}

# Write genesis files into a temp dir that is mounted read-only into the bootstrap.
genesis_files() {
  local dir="$1"
  echo "$VALIDATOR_PUB_HEX $STAKE" > "$dir/bonds.txt"
  : > "$dir/wallets.txt"
}

wait_for_cert() {
  local c="$1"
  for _ in $(seq 1 60); do
    if docker exec "$c" test -f /var/lib/rnode/node.certificate.pem 2>/dev/null; then
      return 0
    fi
    sleep 1
  done
  echo "timed out waiting for $c to generate its TLS cert" >&2
  return 1
}

bootstrap_id() {
  docker exec "$BOOTSTRAP" cat /var/lib/rnode/node.certificate.pem \
    | openssl x509 -noout -subject \
    | awk -F'=' '{print $NF}'
}

cmd_up() {
  local n="${1:-3}"
  if (( n < 1 || n > 5 )); then
    echo "N must be in 1..5" >&2
    exit 2
  fi

  docker network create "$NETWORK" >/dev/null 2>&1 || true

  local genesis_dir
  genesis_dir="$(mktemp -d)"
  genesis_files "$genesis_dir"

  echo "==> starting bootstrap (standalone, creates genesis)"
  docker run -d --name "$BOOTSTRAP" --network "$NETWORK" \
    -p "${GRPC_BASE}:40401" \
    -v "${BOOTSTRAP}-data:/var/lib/rnode" \
    -v "${genesis_dir}:/genesis:ro" \
    "$IMAGE" run -s \
      --host "$BOOTSTRAP" \
      --api-host 0.0.0.0 \
      --data-dir /var/lib/rnode \
      --bonds-file /genesis/bonds.txt \
      --wallets-file /genesis/wallets.txt \
      --validator-private-key "$VALIDATOR_PRIV_HEX" \
      --protocol-port 40400 --discovery-port 40404 \
      --api-port-grpc-internal 40402 --api-port-http 40403 --api-port-admin-http 40405

  wait_for_cert "$BOOTSTRAP"
  local id
  id="$(bootstrap_id)"
  echo "==> bootstrap id: $id"

  for (( i = 1; i < n; i++ )); do
    local name="${PEER_PREFIX}${i}"
    local host_port=$((GRPC_BASE + i * 1000))
    echo "==> starting ${name} (bootstraps from ${BOOTSTRAP})"
    docker run -d --name "$name" --network "$NETWORK" \
      -p "${host_port}:40401" \
      -v "${name}-data:/var/lib/rnode" \
      "$IMAGE" run \
        --host "$name" \
        --api-host 0.0.0.0 \
        --data-dir /var/lib/rnode \
        --bootstrap "rnode://${id}@${BOOTSTRAP}?protocol=40400&discovery=40404" \
        --protocol-port 40400 --discovery-port 40404 \
        --api-port-grpc-internal 40402 --api-port-http 40403 --api-port-admin-http 40405
  done

  echo ""
  echo "==> up. Interact with:"
  echo "    tools/docker-network.sh cli bootstrap <rnode subcommand...>"
  for (( i = 1; i < n; i++ )); do
    echo "    tools/docker-network.sh cli ${PEER_PREFIX}${i} <rnode subcommand...>"
  done
}

cmd_down() {
  local remove_volumes=false
  [[ "${1:-}" == "-v" ]] && remove_volumes=true
  for c in "$BOOTSTRAP" "${PEER_PREFIX}1" "${PEER_PREFIX}2" "${PEER_PREFIX}3" "${PEER_PREFIX}4"; do
    docker rm -f "$c" >/dev/null 2>&1 || true
    if $remove_volumes; then
      docker volume rm "${c}-data" >/dev/null 2>&1 || true
    fi
  done
  docker network rm "$NETWORK" >/dev/null 2>&1 || true
}

cmd_status() {
  docker ps --filter "network=$NETWORK" --format 'table {{.Names}}\t{{.Status}}\t{{.Ports}}'
}

cmd_logs() {
  docker logs -f "${1:?node name required}"
}

cmd_cli() {
  local node="$1"
  shift
  # Run the Rust client *inside* the node container (via `docker exec`) so it can reach the
  # loopback-bound internal propose/repl server (127.0.0.1:40402) as well as the deploy server
  # (0.0.0.0:40401). No --grpc-port is passed: the client defaults to 40401 (deploy) or 40402
  # (repl/propose), matching the node's port split.
  local tty=""
  [[ "${1:-}" == "repl" ]] && tty="-t"
  docker exec -i $tty "$node" rnode --grpc-host localhost "$@"
}

case "${1:-}" in
  build) cmd_build ;;
  up) cmd_up "${2:-3}" ;;
  down) cmd_down "${2:-}" ;;
  status) cmd_status ;;
  logs) cmd_logs "${2:-}" ;;
  cli) shift; cmd_cli "$@" ;;
  *) usage ;;
esac
