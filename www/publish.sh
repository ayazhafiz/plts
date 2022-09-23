#!/bin/sh

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"
dune build
yarn install
cd www && yarn build
cp -R public /tmp/plts && cd ..

git reset --hard HEAD
git checkout gh-pages
echo ".yarn" > .gitignore
echo ".cache" >> .gitignore
rm -rf * .yarn .cache
cp -v -a /tmp/plts/. .
git add -f .

if git commit -m "Update plts" ; then
  git push -f origin HEAD
  echo -e "Deploy completed\n"
else
  echo -e "Content not changed, nothing to deploy\n"
fi          
