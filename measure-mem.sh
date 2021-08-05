#!/bin/bash

################################################################################
#
# Slightly rough way to measure memory
#
# NOTES:
#
# * This adds comment lines to source files; probably wise to make sure the
#   git tree is clean before running.
#
# * Enable building all modules and compile with -O0. In cabal.project.local:
#
#     package large-records
#       flags: +build-all-modules
#       ghc-options: -O0
#
################################################################################

# Reset statistics
rm -f memory.log

# Full cabal build, to start
rm -rf dist-newstyle
cabal build --with-ghc=./timed-ghc.sh

# The loop over 1..10 on the outside is intentional: if that loop would be on
# the inside, any temporary effects on the machine could disproportionally
# affect any single one file.

for i in `seq 1 100`
do
  for j in test/Test/Record/Size/Before/* test/Test/Record/Size/After/*
  do
    echo "$i $j" >> memory.log
    echo "-- Recompile me $i" >> $j
    cabal build --with-ghc=./timed-ghc.sh
  done
done


