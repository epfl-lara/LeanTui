#!/usr/bin/env sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
literatedocbuild_dir="$project_root/literatedocbuild"
html_dir="$project_root/.lake/build/literate-html"
entry_page="$html_dir/Demo/ScreenEdit/ScreenEditDemo/index.html"
scroll_fix_css="$literatedocbuild_dir/verso-scroll-fix.css"
config_file="$project_root/literate.toml"

if [ ! -f "$literatedocbuild_dir/lake-manifest.json" ]; then
  echo "Initializing pinned Verso dependencies..."
  (cd "$literatedocbuild_dir" && lake update)
fi

if [ ! -f "$config_file" ]; then
  echo "Missing Verso literate configuration: $config_file" >&2
  exit 1
fi

echo "Building literate HTML for Demo.ScreenEdit and LeanTui..."
(cd "$literatedocbuild_dir" && lake query @LeanTui:literateHtml)

if [ ! -f "$entry_page" ]; then
  echo "Verso did not generate the expected ScreenEditDemo HTML page." >&2
  exit 1
fi

generated_override_css="$html_dir/$(basename "$scroll_fix_css")"
split_js="$literatedocbuild_dir/verso-split-definitions.js"
generated_split_js="$html_dir/$(basename "$split_js")"

# Verso's facet tracks literate.toml but not changes to files named by extra_css
# and extra_js, so explicitly synchronize those assets on every invocation.
cp "$scroll_fix_css" "$generated_override_css"
cp "$split_js" "$generated_split_js"

echo "Literate documentation built at:"
echo "  $entry_page"
echo "Serve it locally with:"
echo "  python3 -m http.server 8000 -d '$html_dir'"
echo "Then open:"
echo "  http://localhost:8000/Demo/ScreenEdit/ScreenEditDemo/"
