# mutr.r
#
# Minimal test framework
# inspired by https://jera.com/techinfo/jtns/jtn002 and https://github.com/siu/minunit


#' Create, initialize, and deinitialize mutr object
#'
#' @description
#' Create, initialize, and deinitialize a mutr object.
#'
#' @details
#' This objects stores information required to track tests, test sets, and their error messages.
#'
#' `new_mutr` creates a new object, `init_mutr` saves it and `deinit_mutr` removes it from the
#' user's global environment. Before the removal, `deinit_mutr` also prints test results.
#'
#' The `print` argument specifies when the errors are being printed,
#' whether after each `test`, `test_set`, on exit during de-initialization.
#' These are handled in `test`, `test_set`,`test_file`, and `test_dir`.
#'
#' Methods:
#'
#' * add_test(x) -- append test result 'x' to the memory
#' * add_set() -- increase the set ounter by one
#'
#' Slots:
#'
#' * sets -- the total number of test sets, see `[test_set]`
#' * tests -- the total number of tests
#' * failed -- the number of failed tests
#' * print -- when to print errors, see details
#'
#' @param print when to print errors, see details
#' @return `new_mutr` returns an environment containing callable methods,
#'         `init_mutr` and `deinit_mutr` are run for side-effects.
#'
#' @export
new_mutr = function(print = c("test", "set", "exit")){
    sets = 0
    tests = 0
    failed = 0
    print = match.arg(print)
    messages = new_stack()

    add_test = function(x){
        tests <<- tests + 1L

        if(!isTRUE(x)){
            failed <<- failed + 1L
            }
        }

    add_set = function(){
        sets <<- sets + 1L
        }

    structure(environment(), "class" = "counter")
    }


#' @rdname new_mutr
#' @export
init_mutr = function(print){
    mutr = new_mutr(print)
    env = globalenv()
    env$.mutr = mutr

    invisible()
    }


#' @rdname new_mutr
#' @export
deinit_mutr = function(print = FALSE){
    env = globalenv()$.mutr
    rm(".mutr", envir = globalenv())

    if(print){
        cat(
            "[",
            env$sets, "sets,",
            env$tests, "tests:",
            env$failed, "failed,",
            env$tests - env$failed, "passed",
            "]\n\n"
            )

        if(env$failed != 0 && env$print == "exit")
            cat(unlist(env$messages$pop()))
        if(env$failed != 0)
            stop("Some tests did not pass.\n", call. = FALSE)
        }

    invisible()
    }


#' Test expression
#'
#' Test provided expression for true-ness, the evaluated expression is returned invisibly.
#' If the `.mutr` object exists, the test counter is increased and the error message is appended.
#' Otherwise, the error message is printed.
#'
#' Any other condition than `TRUE` results in a differe with different diagnostic expression
#' depending on the evaluation. For this purpose, `test` distinguishes between:
#'
#' * error condition
#' * non-logical object
#' * logical object with length > 1
#' * singular logical FALSE
#' * singular logical TRUE
#'
#' @param expr an expression to be evaluated
#' @return an evaluated expression
#'
#' @export
#'
#' @examples
#' test(TRUE)
#' test(FALSE)
#' test(stop())
test = function(expr){
    res = try(expr, silent = TRUE)
    expr = paste0(deparse(substitute(expr)), collapse = "")

    if(isTRUE(res)){
        msg = paste0("Passed: ", expr, " is TRUE", "\n")
        } else if(class(res)[1] == "try-error"){
        msg = paste0("Error in ", expr,
            ": ", attr(res, "condition")$message, "\n")
        } else if(!is.logical(res)){
        msg = paste0("Error in ", expr,
            ": does not evaluate to TRUE/FALSE\n")
        } else if(length(res) > 1){
        msg = paste0("Error in ", expr,
            ": condition has length > 1\n")
        } else if(!res){
        msg = paste0("Error in ", expr, ": is not TRUE\n")
        } else {
        stop("Unknown condition in ", expr, ": ", res, "\n")
        }

    # TODO this could be simplified
    if(exists(".mutr", envir = globalenv(), mode = "environment")){
        env = globalenv()$.mutr
        env$add_test(res)

        if(env$print == "test"){
            cat(msg)
            } else {
            if(!isTRUE(res)) env$messages$push(msg)
            }

        } else {
        cat(msg)
        }

    invisible(res)
    }


#' Test context
#'
#' Run the `test` functions in a context, such as set, file, or a directory.
#'
#' These function establish context for tests so that a structured test-suite can be constructed.
#' Each of the context functions `test_set`, `test_file`, and `test_dir` initialize the `.mutr`
#' object if it doesn't already exists, and create a `on.exit` destructor to print the test
#' result.
#'
#' @param msg a name of the set
#' @param expr an expression containing `test`s, since only the `mutr::test` functions append
#' to the `.mutr` object, any other expressions won't influence the test counter.
#' @param file a file containing `test_set`s and `test`s
#' @param dir a directory containing files starting with `test`
#' @name test_context

#' @rdname test_context
#' @export
#'
test_set = function(msg, expr){
    if(!exists(".mutr", envir = globalenv(), mode = "environment")){
        init_mutr("set")
        on.exit(deinit_mutr(print = TRUE), add = TRUE)
        }
    env = globalenv()$.mutr
    env$add_set()

    before = env$failed
    res = try(expr, silent = TRUE)
    set_failed = env$failed > before

    cat("  ", msg, ": ", sep = "")
    if(class(res)[1] == "try-error" || set_failed) cat("FAIL\n") else cat("PASS\n")

    if(env$print == "set" && set_failed) cat(unlist(env$messages$pop()))

    invisible()
    }


#' @rdname test_context
#' @export
test_file = function(file){
    if(!exists(".mutr", envir = globalenv(), mode = "environment")){
            init_mutr("exit")
            on.exit(deinit_mutr(print = TRUE), add = TRUE)
            }

    source(file, chdir = TRUE)

    invisible()
    }



#' @rdname test_context
#' @export
test_dir = function(dir){
    if(!exists(".mutr", envir = globalenv(), mode = "environment")){
                init_mutr("exit")
                on.exit(deinit_mutr(print = TRUE), add = TRUE)
                }
    files = dir(dir, "^test[^.]*\\.[rR]$", full.names = TRUE)
    lapply(files, test_file)

    invisible()
    }
