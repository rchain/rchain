#!/usr/bin/env bash
# Regenerate the Scala ground-truth golden vectors consumed by the Rust differential tests.
#
# These vectors are the *oracle*: they are produced by the Scala node (ground truth) and committed
# under crates/<crate>/testdata/differential/. The Rust #[cfg(test)] differential modules assert the
# Rust port reproduces them byte-for-byte (AGENTS.md "translation contract", differential tests).
#
# Requires sbt (available in CI via .github/workflows/continuous-integration.yml; the dev
# environment has no sbt). After regenerating, re-run `cargo test` to confirm the port still matches.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "==> stable_hash.tsv (rspace StableHashProvider)"
sbt "rspace/Test/runMain coop.rchain.rspace.differential.StableHashOracle" \
  > "$ROOT/crates/rspace/testdata/differential/stable_hash.tsv"

echo "==> scodec.tsv (rspace ScodecSerialize byte-level codecs)"
sbt "rspace/Test/runMain coop.rchain.rspace.differential.ScodecOracle" \
  > "$ROOT/crates/rspace/testdata/differential/scodec.tsv"

# wire.tsv (models: the locallyFree BitSet TypeMapper and the empty-Par wire bytes) is derived from
# the shared RhoTypes.proto schema plus the scalapb TypeMapper `bitSetToByteString`; a dedicated
# Scala oracle generator is a follow-up.

echo "Golden vectors regenerated. Re-run: cd crates && cargo test"
