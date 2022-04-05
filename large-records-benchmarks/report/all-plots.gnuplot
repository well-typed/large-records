# Generate all plots for the large-records performance report

set datafile separator ','
set terminal png
set key top left

###
### large-records
###

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

## Benchmark: "HigherKinded"

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/benchmark-02-higherkinded-coresize.png"
plot "<(cat coresize.csv | grep HigherKinded | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "Higher-kinded (ds-preopt)" \
   , "<(cat coresize.csv | grep HigherKinded | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "Higher-kinded (ds)" \
   , "<(cat coresize.csv | grep HigherKinded | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "Higher-kinded (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/benchmark-03-higherkinded-timing.png"
plot "<(cat timing.csv | grep HigherKinded)" using 2:3 with lines lt rgb "#22EE22" title "Higher-kinded"

## Benchmark: "HasNormalForm"

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/benchmark-04-hasnormalform-coresize-pre-verysimpleopt.png"
plot "<(cat coresize.csv | grep HasNormalForm | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "HasNormalForm (ds-preopt)"
set output "graphs/benchmark-05-hasnormalform-coresize-post-verysimpleopt.png"
plot "<(cat coresize.csv | grep HasNormalForm | grep ds,)"    using 2:7 with lines lt rgb "#22EE22" title "HasNormalForm (ds)" \
   , "<(cat coresize.csv | grep HasNormalForm | grep simpl,)" using 2:7 with lines lt rgb "#22EE22" title "HasNormalForm (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/benchmark-06-hasnormalform-timing.png"
plot "<(cat timing.csv | grep HasNormalForm)" using 2:3 with lines lt rgb "#22EE22" title "HasNormalForm"

###
### EXPERIMENTS
###

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

###
### TYPELET
###

## TypeLet: HList

set xlabel "List size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/typelet-hlist-coresize.png"
plot "<(cat coresize.csv | grep HListBaseline  | grep ds-preopt,)" using 2:7 with lines lt rgb "#EE2222" title "Baseline (ds-preopt)" \
   , "<(cat coresize.csv | grep HListBaseline  | grep ds,)"        using 2:7 with lines lt rgb "#EE2222" title "Baseline (ds)" \
   , "<(cat coresize.csv | grep HListBaseline  | grep simpl,)"     using 2:7 with lines lt rgb "#EE2222" title "Baseline (simpl)" \
   , "<(cat coresize.csv | grep HListLetAsCase | grep ds-preopt,)" using 2:7 with lines lt rgb "#2222EE" title "LetAsCase (ds-preopt)" \
   , "<(cat coresize.csv | grep HListLetAsCase | grep ds,)"        using 2:7 with lines lt rgb "#2222EE" title "LetAsCase (ds)" \
   , "<(cat coresize.csv | grep HListLetAsCase | grep simpl,)"     using 2:7 with lines lt rgb "#2222EE" title "LetAsCase (simpl)" \
   , "<(cat coresize.csv | grep HListLetAsCPS  | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "LetAsCPS (ds-preopt)" \
   , "<(cat coresize.csv | grep HListLetAsCPS  | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "LetAsCPS (ds)" \
   , "<(cat coresize.csv | grep HListLetAsCPS  | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "LetAsCPS (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/typelet-hlist-timing.png"
plot "<(cat timing.csv | grep HListBaseline)"  using 2:3 with lines lt rgb "#EE2222" title "Baseline" \
   , "<(cat timing.csv | grep HListLetAsCase)" using 2:3 with lines lt rgb "#2222EE" title "LetAsCase" \
   , "<(cat timing.csv | grep HListLetAsCPS)"  using 2:3 with lines lt rgb "#22EE22" title "LetAsCPS"

## TypeLet: Ap

set xlabel "List size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/typelet-ap-coresize.png"
plot "<(cat coresize.csv | grep ApBaseline | grep ds-preopt,)" using 2:7 with lines lt rgb "#EE2222" title "Baseline (ds-preopt)" \
   , "<(cat coresize.csv | grep ApBaseline | grep ds,)"        using 2:7 with lines lt rgb "#EE2222" title "Baseline (ds)" \
   , "<(cat coresize.csv | grep ApBaseline | grep simpl,)"     using 2:7 with lines lt rgb "#EE2222" title "Baseline (simpl)" \
   , "<(cat coresize.csv | grep ApLet      | grep ds-preopt,)" using 2:7 with lines lt rgb "#2222EE" title "Let (ds-preopt)" \
   , "<(cat coresize.csv | grep ApLet      | grep ds,)"        using 2:7 with lines lt rgb "#2222EE" title "Let (ds)" \
   , "<(cat coresize.csv | grep ApLet      | grep simpl,)"     using 2:7 with lines lt rgb "#2222EE" title "Let (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/typelet-ap-timing.png"
plot "<(cat timing.csv | grep ApBaseline)" using 2:3 with lines lt rgb "#EE2222" title "Baseline" \
   , "<(cat timing.csv | grep ApLet)"      using 2:3 with lines lt rgb "#2222EE" title "Let"

###
### LARGE-ANON
###

## large-anon: construction

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-construct-coresize.png"
plot "<(cat coresize.csv | grep ConstructNoTypeLet   | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "Without TypeLet (ds-preopt)" \
   , "<(cat coresize.csv | grep ConstructNoTypeLet   | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "Without TypeLet (ds)" \
   , "<(cat coresize.csv | grep ConstructNoTypeLet   | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "Without TypeLet (simpl)" \
   , "<(cat coresize.csv | grep ConstructWithTypeLet | grep ds-preopt,)" using 2:7 with lines lt rgb "#2222EE" title "With TypeLet (ds-preopt)" \
   , "<(cat coresize.csv | grep ConstructWithTypeLet | grep ds,)"        using 2:7 with lines lt rgb "#2222EE" title "With TypeLet (ds)" \
   , "<(cat coresize.csv | grep ConstructWithTypeLet | grep simpl,)"     using 2:7 with lines lt rgb "#2222EE" title "With TypeLet (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-construct-timing.png"
plot "<(cat timing.csv | grep ConstructNoTypeLet)"   using 2:3 with lines lt rgb "#22EE22" title "Without TypeLet" \
   , "<(cat timing.csv | grep ConstructWithTypeLet)" using 2:3 with lines lt rgb "#2222EE" title "With TypeLet"

## large-anon: field access

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-get-coresize.png"
plot "<(cat coresize.csv | grep ^GetEvens | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "Field access (ds-preopt)" \
   , "<(cat coresize.csv | grep ^GetEvens | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "Field access (ds)" \
   , "<(cat coresize.csv | grep ^GetEvens | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "Field access (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-get-timing.png"
plot "<(cat timing.csv | grep ^GetEvens)" using 2:3 with lines lt rgb "#22EEEE" title "Field access"

## large-anon: field override (many fields)

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-set-coresize.png"
plot "<(cat coresize.csv | grep ^SetEvens | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "Field override (ds-preopt)" \
   , "<(cat coresize.csv | grep ^SetEvens | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "Field override (ds)" \
   , "<(cat coresize.csv | grep ^SetEvens | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "Field override (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-set-timing.png"
plot "<(cat timing.csv | grep ^SetEvens)" using 2:3 with lines lt rgb "#22EEEE" title "Field override"

## large-anon: update single field

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-updateone-coresize.png"
plot "<(cat coresize.csv | grep ^UpdateOne | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "Update single field (ds-preopt)" \
   , "<(cat coresize.csv | grep ^UpdateOne | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "Update single field (ds)" \
   , "<(cat coresize.csv | grep ^UpdateOne | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "Update single field (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-updateone-timing.png"
plot "<(cat timing.csv | grep ^UpdateOne)" using 2:3 with lines lt rgb "#22EEEE" title "Update single field"

## large-anon: toJSON

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-toJSON-coresize.png"
plot "<(cat coresize.csv | grep ^ToJSON | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "toJSON (ds-preopt)" \
   , "<(cat coresize.csv | grep ^ToJSON | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "toJSON (ds)" \
   , "<(cat coresize.csv | grep ^ToJSON | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "toJSON (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-toJSON-timing.png"
plot "<(cat timing.csv | grep ^ToJSON)" using 2:3 with lines lt rgb "#22EEEE" title "toJSON"

## large-anon: parseJSON

set xlabel "Record size"
set xrange [0:100]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-parseJSON-coresize.png"
plot "<(cat coresize.csv | grep ^ParseJSON | grep ds-preopt,)" using 2:7 with lines lt rgb "#22EE22" title "parseJSON (ds-preopt)" \
   , "<(cat coresize.csv | grep ^ParseJSON | grep ds,)"        using 2:7 with lines lt rgb "#22EE22" title "parseJSON (ds)" \
   , "<(cat coresize.csv | grep ^ParseJSON | grep simpl,)"     using 2:7 with lines lt rgb "#22EE22" title "parseJSON (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-parseJSON-timing.png"
plot "<(cat timing.csv | grep ^ParseJSON)" using 2:3 with lines lt rgb "#22EEEE" title "parseJSON"

###
### LARGE-ANON VS SUPERRECORD
###
### For all superrecord graphs, we only show "simpl" for the timings, to keep
### benchmarking time in check.
###

set xlabel "Record size"

## superrecord: safe construction
## This dwarves everything else, so we plot it by itself

set xrange [0:40]
set ylabel "Core size (terms + types + coercions)"
set output "graphs/superrecord-construct-coresize.png"
plot "<(cat coresize.csv | grep SR_Construct | grep simpl,)" using 2:7 with lines lt rgb "#EE2222" title "superrecord (simpl)"

set xrange [0:40]
set ylabel "Compilation time (ms)"
set output "graphs/superrecord-construct-timing.png"
plot "<(cat timing.csv | grep SR_Construct)" using 2:3 with lines lt rgb "#EE2222" title "superrecord"

## large-anon: construction, compared to superrecord
## We only show the unsafe record construction here

set xrange [0:80]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-vs-superrecord-construct-coresize.png"
plot "<(cat coresize.csv | grep SR_Unsafe          | grep simpl,)" using 2:7 with lines lt rgb "#EE2222" title "superrecord (unsafe, simpl)" \
   , "<(cat coresize.csv | grep ConstructNoTypeLet | grep simpl,)" using 2:7 with lines lt rgb "#22EE22" title "large-anon (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-vs-superrecord-construct-timing.png"
plot "<(cat timing.csv | grep SR_Unsafe)"          using 2:3 with lines lt rgb "#EE2222" title "superrecord (unsafe)" \
   , "<(cat timing.csv | grep ConstructNoTypeLet)" using 2:3 with lines lt rgb "#22EE22" title "large-anon"

set ylabel "Runtime (s)"
set output "graphs/large-anon-vs-superrecord-construct-runtime.png"
plot "<(cat runtime.csv | grep SR_Unsafe            | tr / ,)" using 2:3 with lines lt rgb "#EE2222" title "superrecord (unsafe)" \
                                                          , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#EE2222' notitle \
   , "<(cat runtime.csv | grep SR_Construct         | tr / ,)" using 2:3 with lines lt rgb "#EEEE22" linewidth 5 title "superrecord (safe)" \
                                                          , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#EE2222' notitle \
   , "<(cat runtime.csv | grep ConstructNoTypeLet   | tr / ,)" using 2:3 with lines lt rgb "#22EE22" title "large-anon (without TypeLet)" \
                                                          , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#22EE22' notitle \
   , "<(cat runtime.csv | grep ConstructNoApply     | tr / ,)" using 2:3 with lines lt rgb "#2222EE" title "large-anon (without applyPending)" \
                                                          , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#2222EE' notitle \
   , "<(cat runtime.csv | grep ConstructWithTypeLet | tr / ,)" using 2:3 with lines lt rgb "#22EEEE" title "large-anon (with TypeLet))" \
                                                          , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#22EEEE' notitle

## large-anon: field access, compared to superrecord

set xrange [0:80]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-vs-superrecord-get-coresize.png"
plot "<(cat coresize.csv | grep SR_GetEvens | grep simpl,)" using 2:7 with lines lt rgb "#EE2222" title "superrecord (simpl)" \
   , "<(cat coresize.csv | grep ^GetEvens   | grep simpl,)" using 2:7 with lines lt rgb "#22EE22" title "large-anon (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-vs-superrecord-get-timing.png"
plot "<(cat timing.csv | grep SR_GetEvens)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
   , "<(cat timing.csv | grep ^GetEvens)"   using 2:3 with lines lt rgb "#22EE22" title "large-anon"

set ylabel "Runtime (s)"
set output "graphs/large-anon-vs-superrecord-get-runtime.png"
plot "<(cat runtime.csv | grep SR_GetEvens        | tr / ,)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
                                                        , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#EE2222' notitle \
   , "<(cat runtime.csv | grep GetEvensNoApply    | tr / ,)" using 2:3 with lines lt rgb "#2222EE" title "large-anon (without applyPending)" \
                                                        , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#2222EE' notitle \
   , "<(cat runtime.csv | grep GetEvensAfterApply | tr / ,)" using 2:3 with lines lt rgb "#22EE22" title "large-anon (after applyPending)" \
                                                        , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#22EE22' notitle \

## large-anon: field override (many fields), compared to superrecord

set xrange [0:80]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-vs-superrecord-set-coresize.png"
plot "<(cat coresize.csv | grep SR_SetEvens | grep simpl,)" using 2:7 with lines lt rgb "#EE2222" title "superrecord (simpl)" \
   , "<(cat coresize.csv | grep ^SetEvens   | grep simpl,)" using 2:7 with lines lt rgb "#22EE22" title "large-anon (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-vs-superrecord-set-timing.png"
plot "<(cat timing.csv | grep SR_SetEvens)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
   , "<(cat timing.csv | grep ^SetEvens)"   using 2:3 with lines lt rgb "#22EE22" title "large-anon"

set ylabel "Runtime (s)"
set output "graphs/large-anon-vs-superrecord-set-runtime.png"
plot "<(cat runtime.csv | grep SR_SetEvens       | tr / ,)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
                                                       , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#EE2222' notitle \
   , "<(cat runtime.csv | grep SetEvensNoApply   | tr / ,)" using 2:3 with lines lt rgb "#2222EE" title "large-anon (without applyPending)" \
                                                       , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#2222EE' notitle \
   , "<(cat runtime.csv | grep SetEvensThenApply | tr / ,)" using 2:3 with lines lt rgb "#22EE22" title "large-anon (followed by applyPending)" \
                                                       , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#22EE22' notitle \

## large-anon: update single field, compared to superrecord

set xrange [0:80]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-vs-superrecord-updateone-coresize.png"
plot "<(cat coresize.csv | grep SR_UpdateOne | grep simpl,)" using 2:7 with lines lt rgb "#EE2222" title "superrecord (simpl)" \
   , "<(cat coresize.csv | grep ^UpdateOne   | grep simpl,)" using 2:7 with lines lt rgb "#22EE22" title "large-anon (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-vs-superrecord-updateone-timing.png"
plot "<(cat timing.csv | grep SR_UpdateOne)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
   , "<(cat timing.csv | grep ^UpdateOne)"   using 2:3 with lines lt rgb "#22EE22" title "large-anon"

set ylabel "Runtime (s)"
set output "graphs/large-anon-vs-superrecord-updateone-runtime.png"
plot "<(cat runtime.csv | grep SR_UpdateOne       | tr / ,)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
                                                       , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#EE2222' notitle \
   , "<(cat runtime.csv | grep UpdateOneNoApply   | tr / ,)" using 2:3 with lines lt rgb "#2222EE" title "large-anon (without applyPending)" \
                                                       , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#2222EE' notitle \
   , "<(cat runtime.csv | grep UpdateOneThenApply | tr / ,)" using 2:3 with lines lt rgb "#22EE22" title "large-anon (followed by applyPending)" \
                                                       , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#22EE22' notitle \

## large-anon: toJSON, compared to superrecord

set xrange [0:80]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-vs-superrecord-toJSON-coresize.png"
plot "<(cat coresize.csv | grep SR_ToJSON | grep simpl,)" using 2:7 with lines lt rgb "#EE2222" title "superrecord (simpl)" \
   , "<(cat coresize.csv | grep ^ToJSON   | grep simpl,)" using 2:7 with lines lt rgb "#22EE22" title "large-anon (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-vs-superrecord-toJSON-timing.png"
plot "<(cat timing.csv | grep SR_ToJSON)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
   , "<(cat timing.csv | grep ^ToJSON)"   using 2:3 with lines lt rgb "#22EE22" title "large-anon"

set ylabel "Runtime (s)"
set output "graphs/large-anon-vs-superrecord-toJSON-runtime.png"
plot "<(cat runtime.csv | grep SR_ToJSON | tr / ,)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
                                               , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#EE2222' notitle \
   , "<(cat runtime.csv | grep ^ToJSON   | tr / ,)" using 2:3 with lines lt rgb "#22EE22" title "large-anon" \
                                               , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#22EE22' notitle \

## large-anon: parseJSON, compared to superrecord

set xrange [0:80]

set ylabel "Core size (terms + types + coercions)"
set output "graphs/large-anon-vs-superrecord-parseJSON-coresize.png"
plot "<(cat coresize.csv | grep SR_ParseJSON | grep simpl,)" using 2:7 with lines lt rgb "#EE2222" title "superrecord (simpl)" \
   , "<(cat coresize.csv | grep ^ParseJSON   | grep simpl,)" using 2:7 with lines lt rgb "#22EE22" title "large-anon (simpl)"

set ylabel "Compilation time (ms)"
set output "graphs/large-anon-vs-superrecord-parseJSON-timing.png"
plot "<(cat timing.csv | grep SR_ParseJSON)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
   , "<(cat timing.csv | grep ^ParseJSON)"   using 2:3 with lines lt rgb "#22EE22" title "large-anon"

set ylabel "Runtime (s)"
set output "graphs/large-anon-vs-superrecord-parseJSON-runtime.png"
plot "<(cat runtime.csv | grep SR_ParseJSON | tr / ,)" using 2:3 with lines lt rgb "#EE2222" title "superrecord" \
                                                  , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#EE2222' notitle \
   , "<(cat runtime.csv | grep ^ParseJSON   | tr / ,)" using 2:3 with lines lt rgb "#22EE22" title "large-anon" \
                                                  , '' using 2:3:($3-$6):($3+$6) with yerrorbars lt rgb '#22EE22' notitle \
