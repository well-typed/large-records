default:

# make sure to build with +use-ghc-dump +build-all-modules
variant-none:
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter HL --file-is-number > hl.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter LI --file-is-number > li.csv

# make sure to build with +use-ghc-dump +build-all-modules +blog2-variant-quadratic
variant-quadratic:
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter TI --file-is-number > ti-quadratic.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter TU --file-is-number > tu-quadratic.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter CF --file-is-number > cf-quadratic.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter GH --file-is-number > gh-quadratic.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter TE --file-is-number > te-nominal.csv

# make sure to build with +use-ghc-dump  +build-all-modules +blog2-variant-logarithmic
variant-logarithmic:
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter TI --file-is-number > ti-logarithmic.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter TU --file-is-number > tu-logarithmic.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter CF --file-is-number > cf-logarithmic.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter GH --file-is-number > gh-logarithmic.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter TE --file-is-number > te-phantom.csv

# make sure to build with +use-ghc-dump +build-all-modules +blog2-variant-sop
variant-sop:
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter GH --file-is-number > gh-sop.csv

images:
	gnuplot blogpost2.gnuplot

