set datafile separator ','
set terminal png
set xlabel "Number of record fields"
set ylabel "Core size (terms + types + coercions)"
set xrange [0:100]

# Simple record

set output "sr.png"
plot "sr.csv" using 1:5 with lines title "Simple record"

# Superclasses

set output "sc.png"
plot "superclasses.csv" using 1:($2+$3+$4) with lines title "Superclasses"

# Zipping

set output "rz.png"
plot "rz.csv" using 1:5 with lines title "Zipping"

# Empty classes

set output "li.png"
plot "li.csv" using 1:5 with lines title "Empty class"

# Pattern synonym

set output "ps.png"
plot "ps.csv" using 1:5 with lines title "Pattern synonym"

# Pattern synonym, no field selectors

set output "ps-nofieldselectors.png"
plot "ps.csv" using 1:5 with lines title "Pattern synonym" \
   , "ps-nofieldselectors.csv" using 1:($2+$3+$4) with lines title "with NoFieldSelectors"
