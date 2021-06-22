#!/bin/bash
# Run from repo root
dune build dnf-plus/ft.js
cp dnf-plus/www/index-template.html dnf-plus/www/index.html
sed -i "" "s/CACHE_BUST/$(git rev-parse HEAD)/g" dnf-plus/www/index.html
