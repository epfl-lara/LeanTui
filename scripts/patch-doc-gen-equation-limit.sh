#!/usr/bin/env sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
base_file="$project_root/.lake/packages/doc-gen4/DocGen4/Process/Base.lean"

if [ ! -f "$base_file" ]; then
  echo "doc-gen4 is not checked out at: $base_file" >&2
  echo "Run ./scripts/build-docs.sh once, then retry." >&2
  exit 1
fi

definition_count=$(grep -Ec '^def equationLimit : Nat := [0-9]+$' "$base_file" || true)
if [ "$definition_count" -ne 1 ]; then
  echo "Expected exactly one numeric equationLimit definition in: $base_file" >&2
  exit 1
fi

current_limit=$(sed -n 's/^def equationLimit : Nat := \([0-9][0-9]*\)$/\1/p' "$base_file")
if [ "$current_limit" = "2000" ]; then
  echo "doc-gen4 equationLimit is already 2000."
  exit 0
fi

perl -pi -e \
  's/^def equationLimit : Nat := [0-9]+$/def equationLimit : Nat := 2000/' \
  "$base_file"

if ! grep -q '^def equationLimit : Nat := 2000$' "$base_file"; then
  echo "Failed to patch equationLimit in: $base_file" >&2
  exit 1
fi

echo "Patched doc-gen4 equationLimit from $current_limit to 2000."
echo "Rebuild the documentation with: ./scripts/build-docs.sh"
