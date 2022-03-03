# Generate all plots for the large-records performance report

set datafile separator ','
set terminal png
set key top left

## Benchmark: "Before vs After"

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/benchmark-00-before-vs-after-coresize.png"
plot "<(cat coresize.csv | grep Before | grep ds-preopt,)" using 2:7 with lines lt rgb "#EE2222" title "Before (ds-preopt)" \
   , "<(cat coresize.csv | grep Before | grep ds,)"        using 2:7 with lines lt rgb "#EE2222" title "Before (ds)" \
   , "<(cat coresize.csv | grep Before | grep simpl,)"     using 2:7 with lines lt rgb "#EE2222" title "Before (simpl)" \
   , "<(cat coresize.csv | grep After  | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "After (ds-preopt)" \
   , "<(cat coresize.csv | grep After  | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "After (ds)" \
   , "<(cat coresize.csv | grep After  | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "After (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/benchmark-01-before-vs-after-timing.png"
plot "<(cat timing.csv | grep Before)" using 2:3 with lines lt rgb "#EE2222" title "Before" \
   , "<(cat timing.csv | grep After)"  using 2:3 with lines lt rgb "#22EE22" title "After"

set xrange [0:1000]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/benchmark-02-after-coresize.png"
plot "<(cat coresize.csv | grep After | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "After (ds-preopt)" \
   , "<(cat coresize.csv | grep After | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "After (ds)" \
   , "<(cat coresize.csv | grep After | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "After (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/benchmark-03-after-timing.png"
plot "<(cat timing.csv | grep After)" using 2:3 with lines lt rgb "#22EE22" title "After"

## Benchmark: "HigherKinded"

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/benchmark-04-higherkinded-coresize.png"
plot "<(cat coresize.csv | grep HigherKinded | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "Higher-kinded (ds-preopt)" \
   , "<(cat coresize.csv | grep HigherKinded | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "Higher-kinded (ds)" \
   , "<(cat coresize.csv | grep HigherKinded | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "Higher-kinded (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/benchmark-05-higherkinded-timing.png"
plot "<(cat timing.csv | grep HigherKinded)" using 2:3 with lines lt rgb "#22EE22" title "Higher-kinded"

## Benchmark: "HasNormalForm"

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/benchmark-06-hasnormalform-coresize-pre-verysimpleopt.png"
plot "<(cat coresize.csv | grep HasNormalForm | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "HasNormalForm (ds-preopt)"
set output "graphs/benchmark-07-hasnormalform-coresize-post-verysimpleopt.png"
plot "<(cat coresize.csv | grep HasNormalForm | grep ds,)"    using 2:7 with lines lt rgb "#22EE22" title "HasNormalForm (ds)" \
   , "<(cat coresize.csv | grep HasNormalForm | grep simpl,)" using 2:7 with lines lt rgb "#22EE22" title "HasNormalForm (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/benchmark-08-hasnormalform-timing.png"
plot "<(cat timing.csv | grep HasNormalForm)" using 2:3 with lines lt rgb "#22EE22" title "HasNormalForm"

## Experiment: Simple record

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/experiment-simplerecord-coresize.png"
plot "<(cat coresize.csv | grep SimpleRecord | grep ds-preopt,)" using 2:7 with lines lt rgb "#222222" title "SimpleRecord (ds-preopt)" \
   , "<(cat coresize.csv | grep SimpleRecord | grep ds,)"        using 2:7 with lines lt rgb "#222222" title "SimpleRecord (ds)" \
   , "<(cat coresize.csv | grep SimpleRecord | grep simpl,)"     using 2:7 with lines lt rgb "#222222" title "SimpleRecord (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/experiment-simplerecord-timing.png"
plot "<(cat timing.csv | grep SimpleRecord)" using 2:3 with lines lt rgb "#222222" title "SimpleRecord"

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

## Experiment: Superclasses

set xlabel "Number of superclasses"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/experiment-superclasses-coresize-ds.png"
plot "<(cat coresize.csv | grep Superclasses | grep ds-preopt,)" using 2:7 with lines lt rgb "#222222" title "Superclasses (ds-preopt)" \
   , "<(cat coresize.csv | grep Superclasses | grep ds,)"        using 2:7 with lines lt rgb "#222222" title "Superclasses (ds)"

set output "graphs/experiment-superclasses-coresize-simpl.png"
plot "<(cat coresize.csv | grep Superclasses | grep simpl,)"     using 2:7 with lines lt rgb "#222222" title "Superclasses (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/experiment-superclasses-timing.png"
plot "<(cat timing.csv | grep Superclasses)" using 2:3 with lines lt rgb "#222222" title "Superclasses"

## Experiment: Applicative chains

set xlabel "Chain length"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/experiment-applicative-coresize.png"
plot "<(cat coresize.csv | grep Applicative | grep ds-preopt,)" using 2:7 with lines lt rgb "#222222" title "Applicative (ds-preopt)" \
   , "<(cat coresize.csv | grep Applicative | grep ds,)"        using 2:7 with lines lt rgb "#222222" title "Applicative (ds)" \
   , "<(cat coresize.csv | grep Applicative | grep simpl,)"     using 2:7 with lines lt rgb "#222222" title "Applicative (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/experiment-applicative-timing.png"
plot "<(cat timing.csv | grep Applicative)" using 2:3 with lines lt rgb "#222222" title "Applicative"

## Experiment: "Induction: List, Tree (Nominal), Tree (Phantom)"

set xlabel "List size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/experiment-induction-coresize.png"
plot "<(cat coresize.csv | grep Induction_List         | grep ds-preopt,)" using 2:7 with lines lt rgb "#EE2222" title "List (ds-preopt)" \
   , "<(cat coresize.csv | grep Induction_List         | grep ds,)"        using 2:7 with lines lt rgb "#EE2222" title "List (ds)" \
   , "<(cat coresize.csv | grep Induction_List         | grep simpl,)"     using 2:7 with lines lt rgb "#EE2222" title "List (simpl)" \
   , "<(cat coresize.csv | grep Induction_Tree_Nominal | grep ds-preopt,)" using 2:7 with lines lt rgb "#2222EE" title "Tree/Nominal (ds-preopt)" \
   , "<(cat coresize.csv | grep Induction_Tree_Nominal | grep ds,)"        using 2:7 with lines lt rgb "#2222EE" title "Tree/Nominal (ds)" \
   , "<(cat coresize.csv | grep Induction_Tree_Nominal | grep simpl,)"     using 2:7 with lines lt rgb "#2222EE" title "Tree/Nominal (simpl)" \
   , "<(cat coresize.csv | grep Induction_Tree_Phantom | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "Tree/Phantom (ds-preopt)" \
   , "<(cat coresize.csv | grep Induction_Tree_Phantom | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "Tree/Phantom (ds)" \
   , "<(cat coresize.csv | grep Induction_Tree_Phantom | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "Tree/Phantom (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/experiment-induction-timing.png"
plot "<(cat timing.csv | grep Induction_List)"         using 2:3 with lines lt rgb "#EE2222" title "List" \
   , "<(cat timing.csv | grep Induction_Tree_Nominal)" using 2:3 with lines lt rgb "#2222EE" title "Tree/Nominal" \
   , "<(cat timing.csv | grep Induction_Tree_Phantom)" using 2:3 with lines lt rgb "#22EE22" title "Tree/Phantom"

## Experiment: "ConstraintFamily: Deep vs Shallow"

set xlabel "List size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/experiment-constraintfamily-coresize-pre-verysimpleopt.png"
plot "<(cat coresize.csv | grep ConstraintFamily_Deep    | grep ds-preopt,)" using 2:7 with lines lt rgb "#EE2222" title "Deep (ds-preopt)" \
   , "<(cat coresize.csv | grep ConstraintFamily_Shallow | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "Shallow (ds-preopt)"
set output "graphs/experiment-constraintfamily-coresize-post-verysimpleopt.png"
plot "<(cat coresize.csv | grep ConstraintFamily_Deep    | grep ds,)"        using 2:7 with lines lt rgb "#EE2222" title "Deep (ds)" \
   , "<(cat coresize.csv | grep ConstraintFamily_Deep    | grep simpl,)"     using 2:7 with lines lt rgb "#EE2222" title "Deep (simpl)" \
   , "<(cat coresize.csv | grep ConstraintFamily_Shallow | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "Shallow (ds)" \
   , "<(cat coresize.csv | grep ConstraintFamily_Shallow | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "Shallow (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/experiment-constraintfamily-timing.png"
plot "<(cat timing.csv | grep ConstraintFamily_Deep)"    using 2:3 with lines lt rgb "#EE2222" title "Deep" \
   , "<(cat timing.csv | grep ConstraintFamily_Shallow)" using 2:3 with lines lt rgb "#22EE22" title "Shallow"

# Experiment: Generics: SOP versus `large-generics`

set xlabel "List size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/experiment-generics-coresize.png"
plot "<(cat coresize.csv | grep Generics_SOP | grep ds-preopt,)" using 2:7 with lines lt rgb "#EE2222" title "SOP (ds-preopt)" \
   , "<(cat coresize.csv | grep Generics_SOP | grep ds,)"        using 2:7 with lines lt rgb "#EE2222" title "SOP (ds)" \
   , "<(cat coresize.csv | grep Generics_SOP | grep simpl,)"     using 2:7 with lines lt rgb "#EE2222" title "SOP (simpl)" \
   , "<(cat coresize.csv | grep Generics_LR  | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "LR (ds-preopt)" \
   , "<(cat coresize.csv | grep Generics_LR  | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "LR (ds)" \
   , "<(cat coresize.csv | grep Generics_LR  | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "LR (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/experiment-generics-timing.png"
plot "<(cat timing.csv | grep Generics_SOP)" using 2:3 with lines lt rgb "#EE2222" title "SOP" \
   , "<(cat timing.csv | grep Generics_LR)"  using 2:3 with lines lt rgb "#22EE22" title "LR"

# Experiment: "PreEval"

set xlabel "List size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/experiment-preeval-coresize.png"
plot "<(cat coresize.csv | grep PreEval_Nominal        | grep ds-preopt,)" using 2:7 with lines lt rgb "#EE2222" title "PreEval/Nominal (ds-preopt)" \
   , "<(cat coresize.csv | grep PreEval_Nominal        | grep ds,)"        using 2:7 with lines lt rgb "#EE2222" title "PreEval/Nominal (ds)" \
   , "<(cat coresize.csv | grep PreEval_Nominal        | grep simpl,)"     using 2:7 with lines lt rgb "#EE2222" title "PreEval/Nominal (simpl)" \
   , "<(cat coresize.csv | grep PreEval_Phantom        | grep ds-preopt,)" using 2:7 with lines lt rgb "#2222EE" title "PreEval/Phantom (ds-preopt)" \
   , "<(cat coresize.csv | grep PreEval_Phantom        | grep ds,)"        using 2:7 with lines lt rgb "#2222EE" title "PreEval/Phantom (ds)" \
   , "<(cat coresize.csv | grep PreEval_Phantom        | grep simpl,)"     using 2:7 with lines lt rgb "#2222EE" title "PreEval/Phantom (simpl)" \
   , "<(cat coresize.csv | grep Induction_Tree_Phantom | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "NoPreEval/Phantom (ds-preopt)" \
   , "<(cat coresize.csv | grep Induction_Tree_Phantom | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "NoPreEval/Phantom (ds)" \
   , "<(cat coresize.csv | grep Induction_Tree_Phantom | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "NoPreEval/Phantom (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/experiment-preeval-timing.png"
plot "<(cat timing.csv | grep PreEval_Nominal)"        using 2:3 with lines lt rgb "#EE2222" title "PreEval/Nominal" \
   , "<(cat timing.csv | grep PreEval_Phantom)"        using 2:3 with lines lt rgb "#2222EE" title "PreEval/Phantom" \
   , "<(cat timing.csv | grep Induction_Tree_Phantom)" using 2:3 with lines lt rgb "#22EE22" title "NoPreEval/Phantom"
