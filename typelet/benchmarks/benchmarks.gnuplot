set datafile separator ','
set terminal png
set xlabel "Number of fields"
set xrange [0:100]

# HList

set ylabel "Core size (terms + types + coercions) after desugaring"

set output "hlist.png"
plot "hlist-baseline.csv"  using 1:5 with lines lw 2 title "Baseline"    \
   , "hlist-letas.csv"     using 1:5 with lines lw 2 title "LetAs"       \
   , "hlist-letas-cps.csv" using 1:5 with lines lw 2 title "LetAs (CPS)"

set ylabel "Core size (terms + types + coercions) after simplifier"

set output "simpl-hlist.png"
plot "simpl-hlist-baseline.csv"  using 1:5 with lines lw 2 title "Baseline"    \
   , "simpl-hlist-letas.csv"     using 1:5 with lines lw 2 title "LetAs"       \
   , "simpl-hlist-letas-cps.csv" using 1:5 with lines lw 2 title "LetAs (CPS)"

# HList

set ylabel "Core size (terms + types + coercions) after desugaring"

set output "ap.png"
plot "ap-baseline.csv" using 1:5 with lines lw 2 title "Baseline" \
   , "ap-let.csv"      using 1:5 with lines lw 2 title "Let"

set ylabel "Core size (terms + types + coercions) after simplifier"

set output "simpl-ap.png"
plot "simpl-ap-baseline.csv" using 1:5 with lines lw 2 title "Baseline" \
   , "simpl-ap-let.csv"      using 1:5 with lines lw 2 title "Let"
