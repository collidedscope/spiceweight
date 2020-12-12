spwt: spiceweight.cr
	crystal build --release --no-debug -o spwt spiceweight.cr

dev: spiceweight.cr
	crystal build -o spwt spiceweight.cr
