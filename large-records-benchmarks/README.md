# Benchmarks for the `large-records` library

This package contains a bunch of benchmarks that measure different aspects of
the `large-records` library.

The goal here is to measure compilation time cost. The primary unit for "cost"
is core size: ultimately this is only a proxy measure for compilation time and
memory usage, of course, but core size we can measure more reliably, and is a
good indicator of both time and memory.

See [report/](report/) for the latest benchmark results. This also contains
an explanation of each individual benchmark.

## Update the report

To update the report, run `update.sh` whilst being in the `report` directory.
This will compile the benchmarks, run tools to extract data from it, and then
run a `gnuplot` script to update the graphs. At the moment this is done with
`ghc` 8.8.4.

The only exception is the `NoFieldSelectors` experiment, which requires `ghc`
9.2.1 or up. If re-running this experiment, be sure to clear `dist-newstyle`
before and after and make the necessary temporary changes to `update.sh`.

## TODOs

- When the TH code has been replaced by a plugin, remove unnecesary LANGUAGE
  pragmas from the benchmarks.
- Similarly, once we have the plugin, we should also include it in the
  empty modules.


