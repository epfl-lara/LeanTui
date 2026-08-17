#!/usr/bin/env sh
project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
docbuild_dir="$project_root/docbuild"
cd $docbuild_dir/.lake/build/doc
ls 
open LeanTui.html
