#!/bin/bash

ghc-dump summarize `find . -name '*.cbor'` | egrep 'dist|desugar|Simplifier' >log

sd '.*((After|Before).*cbor)\n' '$1 $3' log

grep Simplifier log >simplifier.csv
grep desugar    log >desugar.csv

for i in simplifier.csv desugar.csv
do
  sd ' *(Simplifier|desugar).*' '' $i
  sd ' +' '\t' $i
  sort -i $i -o $i
done
