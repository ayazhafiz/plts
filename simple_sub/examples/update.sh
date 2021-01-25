#!/bin/bash
# Run from top level of repository

for f in simple_sub/examples/*.checked; do
  rm "$f"
done

for f in simple_sub/examples/*.txt; do
  # I love fancy colors don't hate me
  printf "\e[1mProcessing $f...\e[0m\n"

  bf="simple_sub/examples/$(basename "$f" .txt)"
  dune build "simple_sub/main.exe"
  dune exec "simple_sub/main.exe" -- "$f" &> "$bf.checked"

  printf "\n\e[4mchecked:\e[0m\n\e[33m"
  cat "$bf.checked"
  printf "\e[0m\n"
done
