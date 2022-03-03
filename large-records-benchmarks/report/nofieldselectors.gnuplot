# Generate all plots for NoFieldSelectors experiment
#
# We separate this out because this is the only experiment we run with 9.2.1.

set datafile separator ','
set terminal png
set key top left

## Experiment: Pattern synonyms vs NoFieldSelectors

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/experiment-patternsynonym-coresize.png"
plot "<(cat coresize.csv | grep PatternSynonym_Default | grep ds-preopt,)" using 2:7 with lines lt rgb "#EE2222" title "Default (ds-preopt)" \
   , "<(cat coresize.csv | grep PatternSynonym_Default | grep ds,)"        using 2:7 with lines lt rgb "#EE2222" title "Default (ds)" \
   , "<(cat coresize.csv | grep PatternSynonym_Default | grep simpl,)"     using 2:7 with lines lt rgb "#EE2222" title "Default (simpl)" \
   , "<(cat coresize.csv | grep PatternSynonym_NFS     | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "NoFieldSelectors (ds-preopt)" \
   , "<(cat coresize.csv | grep PatternSynonym_NFS     | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "NoFieldSelectors (ds)" \
   , "<(cat coresize.csv | grep PatternSynonym_NFS     | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "NoFieldSelectors (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/experiment-patternsynonym-timing.png"
plot "<(cat timing.csv | grep PatternSynonym_Default)" using 2:3 with lines lt rgb "#EE2222" title "Default" \
   , "<(cat timing.csv | grep PatternSynonym_NFS)"     using 2:3 with lines lt rgb "#22EE22" title "NoFieldSelectors"
