#!/usr/bin/env sh
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
docbuild_dir="$project_root/docbuild"
cd $docbuild_dir/.lake/build/doc
python3 -m http.server 8000 &
open http://[::]:8000/Demo/ScreenEdit/ScreenEditDemo.html
