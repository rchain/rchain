#!/usr/bin/env bash
# Run the RNodeRust integration-test suites and print a scenario checklist.
#
# Usage: tools/run-integration-tests.sh [--test-threads N]
#
# The node-level tests (node/tests) bind loopback ports; run them serially to avoid
# port-allocation races.

set -euo pipefail

cd "$(dirname "$0")/.."

THREADS=1
while [[ $# -gt 0 ]]; do
  case "$1" in
    --test-threads) THREADS="$2"; shift 2 ;;
    *) echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

export RUST_BACKTRACE=1

echo "==> node-level integration tests (rchain-node)"
cargo test -p rchain-node --tests -- --test-threads "$THREADS"

echo "==> casper consensus-pipeline + multi-node tests (rchain-casper)"
cargo test -p rchain-casper --tests -- --test-threads "$THREADS"

echo "==> rholang execution tests (rchain-rholang)"
cargo test -p rchain-rholang --tests -- --test-threads "$THREADS"

cat <<'EOF'

Scenario checklist (mirrors legacy/integration-tests/):
  [ ] genesis ceremony boot (single-node slice)
  [ ] HTTP /version, /status, /api/status, /api/blocks
  [ ] gRPC DeployService (doDeploy/getBlock/getBlocks/...)
  [ ] propose flow (block created, deterministic post-state)
  [ ] deploy semantics (invalid rholang / min-phlo / insufficient phlo)
  [ ] REV transfers (wallets)
  [ ] multi-node finalization / fault-tolerance / merge / bonding / slashing
  [x] REPL (rnode repl)
  [x] Dockerisation e2e (tools/docker-network.sh, 1-5 nodes)
EOF
