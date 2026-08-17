#!/usr/bin/env sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
html_dir="$project_root/.lake/build/literate-html"
entry_page="$html_dir/Demo/ScreenEdit/ScreenEditDemo/index.html"
port=${1:-8000}

case "$port" in
  ''|*[!0-9]*)
    echo "Port must be a positive integer: $port" >&2
    exit 2
    ;;
esac

if [ "$port" -lt 1 ] || [ "$port" -gt 65535 ]; then
  echo "Port must be between 1 and 65535: $port" >&2
  exit 2
fi

if [ ! -f "$entry_page" ]; then
  echo "Literate documentation has not been built." >&2
  echo "Run ./scripts/build-literate-docs.sh first." >&2
  exit 1
fi

echo "Serving literate documentation at:"
echo "  http://127.0.0.1:$port/Demo/ScreenEdit/ScreenEditDemo/"
echo "Press Ctrl-C to stop the server."

exec python3 -m http.server "$port" --bind 127.0.0.1 --directory "$html_dir"
