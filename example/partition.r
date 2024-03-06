# partition.r
#
# partition dataset according to factor or factors



#' Divide elements into subgroups
#'
#' Randomly divide elements into subgroups of size `k`.
#'
#' The `group_k` function randomly divides `n` elements into subgroups of size `k`, while
#' the `partition_k` functions performs the same operation for each split defined by the `split` variable.
#' The resulting groups are represented as an integer vector with each integer uniquelly identifying each group.
#'
#' Since it is not guaranteed that the `n` elements will be able to be neatly divide into groups of size `k`,
#' if `partial = TRUE` one group (or subgroup) will have less elements, i.e., the remainder after `n %% k`,
#' and if `partial = FALSE`, the unknown value `NA` is set as a group (or subgroup) identifier instead.
#'
#' Using this `group` vector, any object can then be split into assigned groups using `split(x, groups)`,
#' or use `tapply()` or `aggregate()` to calculate group statistics.
#'
#' @param n the total number of elements
#' @param split a factor or list of factor in the sense understood by the `split()` function.
#' @param k the size of groups
#' @param partial if `TRUE`, partial groups (`size < k`) is generated. Otherwise, `NA` is assigned as a group ID.
#' @param split an optional vector of groupings, the group vector will be generated
#' for every grouping.
#' @return a numerical vector of IDs for each `g` group generated this way, ranging from 1 to the number of groups.
#'
#' group_k(5, 2) |> sort() # 1 1 2 2 3
#'
#' # without `sort()`, the result is simply permutated
#' group_k(5,2)
#'
#' group_k(5, 2, partial = FALSE) |> sort(na.last=TRUE) # 1 1 2 2 NA
#' group_k(5, 2, partial = FALSE)
#'
#' # example of practical usage:
#  # split `iris` into groups of size 10
#' split(iris, group_k(nrow(iris), 10))
#'
#' # calculate frequency of Iris species in the created groups:
#' split(iris, group_k(nrow(iris), 10)) |>
#'     lapply(getElement, "Species") |>
#'     lapply(table) |>
#'     do.call(what=rbind)
#'
#' # using `split`:
#' # same as above, but subgroups are created with respect to the `Species` variable
#' split(iris, partition_k(iris$Species, 10)) |>
#'     lapply(getElement "Species") |>
#'     lapply(table) |>
#'     do.call(what=rbind)
#'
#' @seealso
#' `split()` for splitting object according to a factor
#' `sample()` for permutations or random resampling
group_k = function(n, k, partial = TRUE){
    groups = if(partial)
        ceiling(n / k) |> seq_len()
        else
        floor(n / k) |> seq_len() |> append(NA)


    rep(groups, each = k, length.out = n) |> sample()
    }


#' @rdname group_k
partition_k = function(split, k, partial = TRUE){
    if(typeof(split) != "list")
        split = list(split)
        else
        split = as.list(split)

    l = sapply(split, length) |> unique()
    if(length(l) != 1)
        stop("All `split` elements must have the same length!")

    # create subgroups for every group defined by split variables
    tmp = seq_len(l) |>
        split(split, drop=TRUE) |>
        lapply(\(x) group_k(length(x), k, partial))

    # add unique identifier to every group
    tmp = Map(pasteNA, seq_along(tmp), tmp)

    # rename groups so that each group have simple integer ID
    unsplit(tmp, split, drop=TRUE) |>
        (\(x) factor(x, labels = na.omit(x) |> unique() |> seq_along()))() |>
        as.integer()
    }


#' Paste that preserve NA
#'
#' This is a variation on the traditional version of `paste` and `paste0`.
#' But instead of converting `NA`s into a character string `"NA"` and pasting it with other strings,
#' this version preserve the rule of `NA`s in other operations (addition, multiplication) where `NA`s are preserved
#' throughnout operations.
#'
#' @param ... one or more R objects to be converted to character vectors.
#' @param sep a character string to separate the terms. Not `NA_character_`.
#' @return a character vector obtained by pasting together elements of ...
pasteNA = function(..., sep = " "){
    .pasteNA = function(..., sep = sep){
        ifelse(sapply(list(...), is.na) |> any(), NA, paste(..., sep = sep))
        }

    mapply(..., FUN = .pasteNA, MoreArgs = list(sep = sep), USE.NAMES = FALSE)
    }
