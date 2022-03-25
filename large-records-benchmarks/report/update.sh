#!/bin/bash

set -e

# Run this from the report/ directory.

# During debugging, might want to enable these instead of the cabal commands
# below. They produce less accurate results but avoid recompilation every time
# the script is invoked because the flags remain the same.

# cabal build bench-before      --flags="+profile-coresize +profile-timing"
# cabal build bench-after       --flags="+profile-coresize +profile-timing"
# cabal build bench-experiments --flags="+profile-coresize +profile-timing"
# cabal build bench-typelet     --flags="+profile-coresize +profile-timing"
# cabal build bench-large-anon  --flags="+profile-coresize +profile-timing"
# cabal build bench-superrecord --flags="+profile-coresize +profile-timing"

## Core size

cabal build bench-before       --flags=+profile-coresize
cabal build bench-after        --flags=+profile-coresize
cabal build bench-experiments  --flags=+profile-coresize
cabal build bench-typelet      --flags=+profile-coresize
cabal build bench-large-anon   --flags=+profile-coresize
cabal build bench-superrecord  --flags=+profile-coresize

cabal run parse-coresize -- \
  --dist ../../dist-newstyle \
  --match '.*/(.*)/Sized/R(.*)\.dump-(ds-preopt|ds|simpl)' \
  -o coresize.csv

## Timing

cabal build bench-before       --flags=+profile-timing
cabal build bench-after        --flags=+profile-timing
cabal build bench-experiments  --flags=+profile-timing
cabal build bench-typelet      --flags=+profile-timing
cabal build bench-large-anon   --flags=+profile-timing
cabal build bench-superrecord  --flags=+profile-timing

cabal run parse-timing -- \
  --dist ../../dist-newstyle \
  --match '.*/(.*)/Sized/R(.*)\.dump-timings' \
  --omit-per-phase \
  -o timing.csv

## Plots

gnuplot all-plots.gnuplot
# gnuplot nofieldselectors.gnuplot # from 9.2.1 only
