#!/bin/bash
# Run from repo root
dune build TAL/tal.js
cp tal/www/index-template.html tal/www/index.html
sed -i"" "s/CACHE_BUST/$(git rev-parse HEAD)/g" tal/www/index.html
