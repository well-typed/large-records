default:

csv:
	# ds

	parse-dump-files --dist ../dist-newstyle --match '.*/HList/Baseline/Baseline(.*)\.dump-ds$$' | sort >hlist-baseline.csv
	parse-dump-files --dist ../dist-newstyle --match '.*/HList/LetAs/LetAs(.*)\.dump-ds$$'       | sort >hlist-letas.csv
	parse-dump-files --dist ../dist-newstyle --match '.*/HList/LetAsCPS/LetAsCPS(.*)\.dump-ds$$' | sort >hlist-letas-cps.csv

	parse-dump-files --dist ../dist-newstyle --match '.*/Ap/Baseline/Baseline(.*)\.dump-ds$$'    | sort >ap-baseline.csv
	parse-dump-files --dist ../dist-newstyle --match '.*/Ap/Let/Let(.*)\.dump-ds$$'              | sort >ap-let.csv

	# simpl

	parse-dump-files --dist ../dist-newstyle --match '.*/HList/Baseline/Baseline(.*)\.dump-simpl$$' | sort >simpl-hlist-baseline.csv
	parse-dump-files --dist ../dist-newstyle --match '.*/HList/LetAs/LetAs(.*)\.dump-simpl$$'       | sort >simpl-hlist-letas.csv
	parse-dump-files --dist ../dist-newstyle --match '.*/HList/LetAsCPS/LetAsCPS(.*)\.dump-simpl$$' | sort >simpl-hlist-letas-cps.csv

	parse-dump-files --dist ../dist-newstyle --match '.*/Ap/Baseline/Baseline(.*)\.dump-simpl$$'    | sort >simpl-ap-baseline.csv
	parse-dump-files --dist ../dist-newstyle --match '.*/Ap/Let/Let(.*)\.dump-simpl$$'              | sort >simpl-ap-let.csv

images:
	gnuplot benchmarks.gnuplot

clean:
	rm -f *.csv *.png
