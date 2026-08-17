#!/usr/bin/env sh

set -eu

version=${1:-v4.29.0}

if [ "$#" -gt 1 ]; then
  echo "Usage: $0 [vMAJOR.MINOR.PATCH]" >&2
  exit 2
fi

if ! printf '%s\n' "$version" | grep -Eq '^v[0-9]+\.[0-9]+\.[0-9]+$'; then
  echo "Invalid Lean version '$version'; expected vMAJOR.MINOR.PATCH." >&2
  exit 2
fi

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
docbuild_dir="$project_root/docbuild"

echo "Updating LeanTui projects to Lean $version..."
printf 'leanprover/lean4:%s\n' "$version" > "$project_root/lean-toolchain"
printf 'leanprover/lean4:%s\n' "$version" > "$docbuild_dir/lean-toolchain"

LEAN_VERSION_TO_SET=$version perl -0pi -e \
  's/(name = "doc-gen4"\s+rev = ")[^"]+(".*)/$1$ENV{LEAN_VERSION_TO_SET}$2/' \
  "$docbuild_dir/lakefile.toml"

echo "Refreshing Lake manifests..."
(cd "$project_root" && lake update)
(cd "$docbuild_dir" && lake update)

echo "Verifying the library build..."
(cd "$project_root" && lake build)

echo "Verifying the documentation build..."
"$project_root/scripts/build-docs.sh"

echo "LeanTui projects now use Lean $version."
