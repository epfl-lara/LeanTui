#!/usr/bin/env sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
docbuild_dir="$project_root/docbuild"
doc_data_dir="$docbuild_dir/.lake/build/doc-data"

if [ ! -f "$docbuild_dir/lake-manifest.json" ]; then
  echo "Initializing pinned documentation dependencies..."
  (cd "$docbuild_dir" && lake update doc-gen4)
fi

# doc-gen4 v4.29 can replay stale docInfo after a doc-comment-only edit. Invalidate
# only project markers whose Lean source is newer, leaving the expensive core cache intact.
docs_stale=false
for source in \
  "$project_root"/LeanTui.lean \
  "$project_root"/LeanTui/*.lean \
  "$project_root"/Demo/ScreenEdit.lean \
  "$project_root"/Demo/ScreenEdit/*.lean
do
  [ -f "$source" ] || continue
  relative_path=${source#"$project_root"/}
  module_name=$(printf '%s' "${relative_path%.lean}" | tr '/' '.')
  marker="$doc_data_dir/$module_name.doc"
  if [ -f "$marker" ] && [ "$source" -nt "$marker" ]; then
    echo "Refreshing documentation data for $module_name..."
    rm -f "$marker" "$marker.hash" "$marker.trace"
    docs_stale=true
  fi
done

if [ "$docs_stale" = true ]; then
  docs_marker="$doc_data_dir/Demo.ScreenEdit--library.docs_built"
  rm -f "$docs_marker" "$docs_marker.hash" "$docs_marker.trace"
fi

echo "Building LeanTui and ScreenEditDemo HTML documentation..."
(cd "$docbuild_dir" && lake build Demo.ScreenEdit:docs "$@")

echo "Documentation built at:"
echo "  $docbuild_dir/.lake/build/doc/index.html"
echo "Serve it locally with:"
echo "  cd '$docbuild_dir/.lake/build/doc' && python3 -m http.server 8000"
