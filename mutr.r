# mutr.r
#
# Minimal test framework
# inspired by https://jera.com/techinfo/jtns/jtn002 and https://github.com/siu/minunit


new_stack = function(init = 20L){
    items = vector("list", init)
    size = 0L

    push = function(...){
        new = list(...)
        new_size = length(new) + size
        while(new_size > length(items))
            items[[new_size * 2L]] = list(NULL)

        items[size + seq_along(new)] <<- new
        size <<- new_size

        invisible(NULL)
        }

    pop = function(n = NULL){
        if(is.null(n)) n = size
        size <<- size - n # no need to clean list
        items[size + seq_len(n)]
        }

    structure(environment(), "class" = "stack")
    }
.errors = new_stack()
.errors$print = "test"

#' Test expression
#'
#' Test provided expression for true-ness. Returns `TRUE` only if the expression evaluates
#' to `TRUE`. Any other condition results in `FALSE` with different diagnostic expression
#' depending on the evaluation. For this purpose, `test` distinguishes between:
#'
#' * raised error
#' * non-logical object
#' * logical object with length > 1
#' * singular logical FALSE
#' * singular logical TRUE
#'
#' The return value, be it TRUE or FALSE, is returned invisibly.
test = function(expr){
    res = try(expr, silent=TRUE)

    if(class(res)[1] == "try-error"){
        cat("Error in ", deparse(substitute(expr)),
            ": ", attr(res, "condition")$message, "\n", sep="") 
        return(invisible(FALSE))
        }

    if(!is.logical(res)){
        cat("Error in ", deparse(substitute(expr)),
            ": does not evaluate to TRUE/FALSE\n", sep="")
        return(invisible(FALSE))
        }

    if(length(res) > 1){
        cat("Error in ", deparse(substitute(expr)),
            ": condition has length > 1\n", sep="")
        return(invisible(FALSE))
        }

    if(!res){
        cat("Error in ", deparse(substitute(expr)), ": is not TRUE\n", sep="")
        return(invisible(FALSE))
        }

    invisible(TRUE)
    }


#' Simple test-suite
#'
#' Simple test-suite to collate and evaluate multiple tests, which themselves are passed
#' as multiple arguments. A diagnostic message is printed to show the number of passed/failed
#' tests. In addition, an error is thrown if some some tests did not pass.
#' This error can be optionally turned into warning, message, or turned off completely with
#' `test_suite(..., throw=FALSE)`.
test_suite = function(..., throw=c("error", "warning", "message", FALSE)){
    throw = match.arg(as.character(throw), throw)    

    tests = list(...)
    res = sapply(tests, eval)

    cat("[", length(res), "tests:", sum(!res), "failed,", sum(res), "passed ]\n\n")

    if(!all(res)){
        msg = "Some tests did not pass."
        switch(throw, # unfortunatelly, these functions do not have unified response form
            "error" = stop(msg, call.=FALSE),
            "warning" = warning(msg, call.=FALSE, immediate.=TRUE),
            "message" = message("Note: ", msg),
            "FALSE" = "")
        }

    invisible(all(res)) 
    }


# TODO
# test_set -- set of tests with name
# example:
# test_foo = test_set(...)
# test_bar = test_set(...)
# test_suite(
#     test_foo,
#     test_bar
#     )

#' Test if an object is an error
is_error = function(x){
    inherits(x, c("try-error", "error", "simpleError"))
    }

#' Negate usable in a pipe
not = function(x){
    !x
    }

# Example of usage of test and test-suite
#test_suite(throw="message",
#    test(FALSE),
#    test(TRUE),
#    test(stop("This will throw an error")),
#    test(1)
#    )
