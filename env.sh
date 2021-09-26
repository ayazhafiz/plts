#!/bin/bash

# Wrapper around dune to avoid trying to examine node_modules when running dune
# from root.
dune() {
  if [ "$(git rev-parse --show-toplevel 2>/dev/null)" == "$(pwd)" ] && [ -d "node_modules" ]; then
    mv node_modules _nm; command dune "$@"; mv _nm node_modules
  else
    command dune "$@"
  fi
}
