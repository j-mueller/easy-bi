#! /bin/bash

fdfind --extension hs \
  --exclude 'dist-newstyle/*' \
  --exclude 'dist/*' \
  --exclude '.stack-work/*' \
  --exec bash -c "~/.cabal/bin/stylish-haskell -c stylish-haskell.yaml -i {} || true"