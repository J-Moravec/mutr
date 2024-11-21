.RECIPEPREFIX = +

all: test copy

test:
+ Rscript -e 'source("mutr.r"); source("test-mutr.r")'

copy: test
+ cp mutr.r test-mutr.r examples/helpers/

.PHONY: all test copy
