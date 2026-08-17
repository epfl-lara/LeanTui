#!/usr/bin/env sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
docbuild_dir="$project_root/docbuild"

if [ ! -f "$docbuild_dir/lake-manifest.json" ]; then
  echo "Initializing pinned documentation dependencies..."
  (cd "$docbuild_dir" && lake update doc-gen4)
fi

echo "Building LeanTui and ScreenEditDemo HTML documentation..."
(cd "$docbuild_dir" && lake build LeanTui:docs Demo.ScreenEdit:docs "$@")

echo "Documentation built at:"
echo "  $docbuild_dir/.lake/build/doc/index.html"
echo "Serve it locally with:"
echo "  cd '$docbuild_dir/.lake/build/doc' && python3 -m http.server 8000"
