options(warn = 2) # convert warnings to errors

set_script_dir = function(){
    commandArgs(FALSE)
    dir = commandArgs(FALSE) |>
        grep(pattern="^--file=", value=TRUE) |>
        sub(pattern="^--file=", replacement="") |>
        dirname()
    setwd(dir)
    }

if(sys.nframe() == 0) set_script_dir()

source("../mutr.r")
source("partition.r")

# helper function
n_unique = function(x){
    unique(x) |> length()
    }

message("Testing: group_k")
test_suite(
    # output group identifiers are integers
    test( group_k(5, 2, TRUE) |> is.integer() ),
    test( group_k(5, 2, FALSE) |> is.integer() ),

    # identifiers are consecutive
    test( identical(group_k(5, 2, TRUE) |> unique() |> sort(), 1:3) ),
    test( identical(group_k(5, 2, FALSE) |> unique() |> sort(), 1:2) ),

    # if partial = FALSE, the number of NAs is equal to n %% k
    test( identical( as.integer(11 %% 3), group_k(11, 3, FALSE) |> is.na() |> sum()) ),
    test( identical( as.integer(17 %% 6), group_k(17, 6, FALSE) |> is.na() |> sum()) ),

    # if n %% k = 0, or partial = FALSE and NAs are removed
    # then all groups should have the same size
    test( (group_k(12, 3) |> table() |> n_unique()) == 1 ),
    test( (group_k(11, 3, partial = FALSE) |> table() |> n_unique()) == 1 ),

    # otherwise, exactly one group should less elements or NAs will be present
    test( (group_k(11, 3) |> table(useNA = "ifany") |> n_unique()) == 2),
    test( (group_k(11, 3, partial = FALSE) |> table(useNA = "ifany") |> n_unique()) == 2)
    )


message("Testing: partition_k")
test_suite(
    # output group identifiers are integers
    test( partition_k(iris$Species, 2, TRUE) |> is.integer() ),
    test( partition_k(iris$Species, 2, FALSE) |> is.integer() ),

    # identifiers are consecutive
    test(identical(
        partition_k(iris$Species, 7, TRUE) |> unique() |> sort(),
        ( table(iris$Species) / 7 ) |> ceiling() |> sum() |> seq_len()
        )),
    test(identical(
        partition_k(iris$Species, 7, FALSE) |> unique() |> sort(),
        ( table(iris$Species) %/% 7 ) |> sum() |> seq_len()
        )),

    # empty groupings are dropped
    test( partition_k(list(iris$Species, iris$Species), 4) |> is.integer()),

    # accepts data.frame of group factors
    test( partition_k(iris["Species"], 4) |> is.integer()),

    # Fixed warning due to incorrect assignment into groups
    # split(drop=TRUE) must be followed by unsplit(drop=TRUE)
    # otherwise, elements are being assigned to incorrect groups
    # this results in warning, but only if the size of the groups
    # differs
    test( partition_k(list( rep(1:5, times = 1:5), rep(1:3, each=5)), 4) |> is.integer())
    )
