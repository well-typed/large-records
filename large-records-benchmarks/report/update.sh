#!/bin/bash

set -e

#
# Run this from the report/ directory.
#
# To benchmark only coresize, timing, or time, set the BENCH env var.
#


## Core size

if [[ "$BENCH" == "" || "$BENCH" == "coresize" ]]
then

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

fi

## Timing

if [[ "$BENCH" == "" || "$BENCH" == "timing" ]]
then

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

fi

## Runtime

rm -f runtime.csv

if [[ "$BENCH" == "" || "$BENCH" == "runtime" ]]
then

  cabal build bench-large-anon  --flags=+profile-runtime
  cabal build bench-superrecord --flags=+profile-runtime

  cabal run bench-large-anon  -- --csv runtime.csv
  cabal run bench-superrecord -- --csv runtime.csv

fi

## Plots

gnuplot all-plots.gnuplot
# gnuplot nofieldselectors.gnuplot # from 9.2.1 only
