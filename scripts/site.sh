#!/bin/bash

set -eu
set -o xtrace

with_www_dirs() {
  for f in **/www; do
    # lang_narrow lives outside our tree
    if [ "$f" != "lang_narrow/www" ]; then
      $1 $f
    fi
  done
}

build() {
  "./$1/build.sh"
}

copy_out() {
  proj_name="$(dirname "$1")"
  out_dir="$OUT/$proj_name"
  mkdir -p "$out_dir"
  cp -v -R "$1" "$out_dir"
}

case "$1" in
  build-all) 
    with_www_dirs build
    ;;
  copy-out)
    OUT="$2"
    mkdir -p "$OUT"
    cp -v index.md "$OUT"
    with_www_dirs copy_out
    ;;
  *)
    echo "unknown option $1"
    exit 1
    ;;
esac
