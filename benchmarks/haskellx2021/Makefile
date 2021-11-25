default:

# make sure to build with +use-ghc-dump +build-all-modules
variant-none:
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter SR --file-is-number > sr.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter RZ --file-is-number > rz.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter LI --file-is-number > li.csv
	ghc-dump summarize `find ../.. -name '*-0000.cbor'` | parse-ghc-dump-summary --sort --filter PS --file-is-number > ps.csv

images:
	gnuplot haskellx.gnuplot

