#!/bin/bash
# Run from repo root
dune build TAL/tal.js
cp TAL/www/index-template.html TAL/www/index.html
sed -i "" "s/CACHE_BUST/$(git rev-parse HEAD)/g" TAL/www/index.html
