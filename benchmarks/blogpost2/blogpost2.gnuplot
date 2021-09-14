set datafile separator ','
set terminal png
set xlabel "Number of record fields"
set ylabel "Core size (terms + types + coercions)"
set xrange [0:100]

##
## Comments refer to the sections of the blog post
##

# Recap: The problem of type arguments

set output "hl.png"
plot "hl.csv" using 1:5 with lines title "exampleValue"

# Instance induction considered harmful

set output "li.png"
plot "li.csv" using 1:5 with lines title "List induction" \
   , "hl.csv" using 1:5 with lines title "exampleValue"

# Towards a solution

set output "ti-quadratic.png"
plot "ti-quadratic.csv" using 1:5 with lines title "Tree induction" \
   , "li.csv"           using 1:5 with lines title "List induction"

# Six or half a dozen

set output "tu-logarithmic.png"
plot "ti-quadratic.csv"   using 1:5 with lines title "ToTree at def site" \
   , "tu-logarithmic.csv" using 1:5 with lines title "ToTree at use site"

set output "tu-quadratic.png"
plot "tu-quadratic.csv"   using 1:5 with lines title "ToTree at use site (using type application)" \
   , "ti-quadratic.csv"   using 1:5 with lines title "ToTree at def site"            \
   , "tu-logarithmic.csv" using 1:5 with lines title "ToTree at use site (using proxy)"

# Solving the problem

set output "ti-logarithmic.png"
plot "ti-quadratic.csv"   using 1:5 with lines title "Tree induction (nominal role)" \
   , "ti-logarithmic.csv" using 1:5 with lines title "Tree induction (phantom role)"

# Constraint families

set output "cf-quadratic.png"
plot "cf-quadratic.csv" using 1:5 with lines title "Constraint family"

# Avoiding deep normalization

set output "cf-logarithmic.png"
plot "cf-quadratic.csv"   using 1:5 with lines title "Constraint family without class alias" \
   , "cf-logarithmic.csv" using 1:5 with lines title "Constraint family using class alias"

# Putting it all together

set output "gh.png"
plot "gh-sop.csv"         using 1:5 with lines title "SOP generics"              \
   , "gh-quadratic.csv"   using 1:5 with lines title "LR generics, nominal role" \
   , "gh-logarithmic.csv" using 1:5 with lines title "LR generics, phantom role"

# Postscript: Pre-evaluating type families

set output "te.png"
plot "te-phantom.csv" using 1:5 title "Phantom role" \
   , "te-nominal.csv" using 1:5 title "Nominal role"

