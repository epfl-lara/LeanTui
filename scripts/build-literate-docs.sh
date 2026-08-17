#!/usr/bin/env sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
literatedocbuild_dir="$project_root/literatedocbuild"
literate_dir="$literatedocbuild_dir/.lake/build/literate"
html_dir="$literatedocbuild_dir/.lake/build/literate-html"
entry_page="$html_dir/Demo/ScreenEdit/ScreenEditDemo/index.html"
generated_css="$html_dir/code.css"
scroll_fix_css="$literatedocbuild_dir/verso-scroll-fix.css"

if [ ! -f "$literatedocbuild_dir/lake-manifest.json" ]; then
  echo "Initializing pinned Verso dependencies..."
  (cd "$literatedocbuild_dir" && lake update)
fi

echo "Generating literate data for Demo.ScreenEdit and its LeanTui dependency..."
(cd "$literatedocbuild_dir" && \
  lake build @LeanTui/LeanTui:literate @LeanTui/Demo.ScreenEdit:literate)

if [ ! -f "$literate_dir/Demo/ScreenEdit/ScreenEditDemo.json" ] || \
   [ ! -f "$literate_dir/LeanTui.json" ]; then
  echo "Verso did not generate the expected ScreenEditDemo and LeanTui data." >&2
  exit 1
fi

echo "Rendering literate HTML with Verso..."
(cd "$literatedocbuild_dir" && lake exe verso-html "$literate_dir" "$html_dir")

if [ ! -f "$entry_page" ]; then
  echo "Verso did not generate the expected ScreenEditDemo HTML page." >&2
  exit 1
fi

if [ ! -f "$generated_css" ]; then
  echo "Verso did not generate the expected stylesheet: $generated_css" >&2
  exit 1
fi

if ! grep -Fq '/* LeanTui Verso scroll fix */' "$generated_css"; then
  echo "Applying the Verso scrolling fix..."
  printf '\n' >> "$generated_css"
  cat "$scroll_fix_css" >> "$generated_css"
fi

echo "Literate documentation built at:"
echo "  $entry_page"
echo "Serve it locally with:"
echo "  python3 -m http.server 8000 -d '$html_dir'"
echo "Then open:"
echo "  http://localhost:8000/Demo/ScreenEdit/ScreenEditDemo/"
