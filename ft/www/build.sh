#!/bin/bash
# Run from repo root
dune build ft/ft.js
cp ft/www/index-template.html ft/www/index.html
sed -i"" "s/CACHE_BUST/$(git rev-parse HEAD)/g" ft/www/index.html
