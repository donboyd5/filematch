# make matchrecs testdata ----
library(tidyverse)
library(readr)


# test on the same data I used in julia ----
aranks <- read_delim("
ida weighta ranka
4 79.9297 0.045742
1 93.4751 0.410937
8 74.8385 0.518504
7 49.0379 0.624424
10 5.8328 0.630887
5 66.3594 0.729227
9 56.304 0.747482
2 11.9394 0.858867
6 67.1042 0.979532
3 21.3759 0.986625",
delim=" ", col_names = TRUE, trim_ws = TRUE)

branks <- read_delim("
idb weightb rankb
5 32.3814 0.0658773
3 110.878 0.171634
2 117.388 0.219503
4 170.165 0.580475
1 95.3846 0.759547",
delim=" ", col_names = TRUE, trim_ws = TRUE)

expected_matchrecs <- read_delim("
ida idb weight ranka rankb
1 2 30.1455 0.410937 0.2195030
1 3 63.3296 0.410937 0.1716340
2 1 6.9045 0.858867 0.7595470
2 4 5.0349 0.858867 0.5804750
3 1 21.3759 0.986625 0.7595470
4 3 47.5484 0.045742 0.1716340
4 5 32.3813 0.045742 0.0658773
5 4 66.3594 0.729227 0.5804750
6 1 67.1042 0.979532 0.7595470
7 4 36.6339 0.624424 0.5804750
7 2 12.4040 0.624424 0.2195030
8 2 74.8385 0.518504 0.2195030
9 4 56.3040 0.747482 0.5804750
10 4 5.8328 0.630887 0.5804750
")


usethis::use_data(aranks, overwrite = TRUE)
usethis::use_data(branks, overwrite = TRUE)
usethis::use_data(expected_matchrecs, overwrite = TRUE)

