#!/usr/bin/env bash
#
# Autoformat and lint all Haskell files

GIT_ROOT=$(git rev-parse --show-toplevel)
PRJ_ROOT="$GIT_ROOT"

HS_FILES=$(find "$GIT_ROOT" -type f -name '*.hs' ! -path '*/dist-newstyle/*')
stylish-haskell $HS_FILES --inplace

# Exist with non zero status if we're now in unclean state
if [ -z "$(git status --porcelain)" ]; then
    echo "No style errors detected."
else
    errdiff=$(git --no-pager diff --color=always -- ${HS_FILES[@]})
    if [ ! -z "$errdiff" ]; then
      printf "\tStyle errors detected:\n\n"
      echo "$errdiff"
      printf "\n\tPlease run nix/style-check.sh to auto format the code and commit changes."
      exit 1
    fi
fi

hlint "$PRJ_ROOT"
