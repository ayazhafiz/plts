#!/bin/sh

set -x
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"
mv node_modules /tmp/node_modules
dune build
mv /tmp/node_modules node_modules
yarn install
cd www && yarn build
cp -R public /tmp/plts && cd ..

cd ../plts2
git reset --hard HEAD
git checkout gh-pages
rm -rf * .yarn .cache
cp -v -a /tmp/plts/. .
git add -f .

if git commit -m "Update plts" ; then
  git push -f origin HEAD
  echo -e "Deploy completed\n"
else
  echo -e "Content not changed, nothing to deploy\n"
fi