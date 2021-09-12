#!/bin/bash
# Run from repo root
dune build gtlc/gtlc.js
cp gtlc/www/index-template.html gtlc/www/index.html
sed -i"" "s/CACHE_BUST/$(git rev-parse HEAD)/g" gtlc/www/index.html
