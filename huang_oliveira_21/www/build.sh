#!/bin/bash
# Run from repo root
dune build huang_oliveira_21/ho21.js
cp huang_oliveira_21/www/index-template.html huang_oliveira_21/www/index.html
sed -i "" "s/CACHE_BUST/$(git rev-parse HEAD)/g" huang_oliveira_21/www/index.html
