#!/usr/bin/env sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
literatedocbuild_dir="$project_root/literatedocbuild"
html_dir="$project_root/.lake/build/literate-html"
screen_edit_page="$html_dir/Demo/ScreenEdit/ScreenEditDemo/index.html"
chess_page="$html_dir/Demo/Chess/Main/index.html"
editor_page="$html_dir/Demo/Editor/EditorDemo/index.html"
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

echo "Building literate HTML for LeanTui, Demo.ScreenEdit, Demo.Chess, and Demo.Editor..."
(cd "$literatedocbuild_dir" && lake query @LeanTui:literateHtml)

if [ ! -f "$screen_edit_page" ]; then
  echo "Verso did not generate the expected ScreenEdit HTML page." >&2
  exit 1
fi

if [ ! -f "$chess_page" ]; then
  echo "Verso did not generate the expected Chess HTML page." >&2
  exit 1
fi

if [ ! -f "$editor_page" ]; then
  echo "Verso did not generate the expected Editor HTML page." >&2
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
echo "  $html_dir"
echo "Serve it locally with:"
echo "  python3 -m http.server 8000 -d '$html_dir'"
echo "Example pages:"
echo "  http://localhost:8000/Demo/ScreenEdit/ScreenEditDemo/"
echo "  http://localhost:8000/Demo/Chess/Main/"
echo "  http://localhost:8000/Demo/Editor/EditorDemo/"
