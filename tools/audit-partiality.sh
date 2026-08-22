#!/usr/bin/env bash
# Audit the Rust port for silent-partiality patterns.
#
# A re-runnable grep over the production `src/` trees. It is a *candidate* finder, not a
# test-excluding classifier: lines inside `#[cfg(test)] mod tests` blocks are filtered heuristically
# (dropped when the block marker or a `// test`-style comment is on/near the line), but the
# authoritative production/test split is the manual catalogue in spec/TYPE-SYSTEM.md.
#
# Usage: tools/audit-partiality.sh [pattern-class ...]
#   pattern-classes: panic, cast, get  (default: all three)

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CRATES=(sdk shared crypto graphz models block-storage comm rspace rholang casper regex node)
SRC_DIRS=()
for c in "${CRATES[@]}"; do
  SRC_DIRS+=("$ROOT/$c/src")
done

classes=("${@:-panic cast get}")
grep_exclude='mod tests|#\[cfg\(test\)\]|#\[test\]|mod property_tests|// test'

for cls in "${classes[@]}"; do
  echo "===== class: $cls ====="
  case "$cls" in
    panic)
      grep -rnE '\.unwrap\(\)|\.expect\(|panic!|unreachable!|todo!|unimplemented!' \
        "${SRC_DIRS[@]}" 2>/dev/null | grep -vE "$grep_exclude" || true
      ;;
    cast)
      # narrowing / signedness-change / truncating numeric casts
      grep -rnE '\bas (i8|i16|i32|u8|u16|u32|u64|usize|isize)\b' \
        "${SRC_DIRS[@]}" 2>/dev/null | grep -vE "$grep_exclude" || true
      ;;
    get)
      grep -rnE '\.get\([^)]*\)\.(unwrap|expect)\(|\.(next|last|first|pop)\(\)\.(unwrap|expect)\(|\b[a-zA-Z_]+\[[0-9]+\]' \
        "${SRC_DIRS[@]}" 2>/dev/null | grep -vE "$grep_exclude" || true
      ;;
    *)
      echo "unknown class: $cls (expected panic|cast|get)" >&2
      exit 1
      ;;
  esac
done
