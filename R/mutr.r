# mutr.r
#
# Minimal test framework
# inspired by https://jera.com/techinfo/jtns/jtn002 and https://github.com/siu/minunit

#' Create new stack object
#'
#' @description
#' Create a new stack object with reference semantics and two methods,
#' `push` adds objects to the stack and `pop` removes them.
#'
#' @details
#' This is dependency-free class based on function closures with reference semantics.
#' Internally, the stack is represented as a pre-allocated `list` that is extended as required.
#' The size of the `list` is set to the `init`, and is dynamically extended by the `init` value.
#'
#' Following methods are available:
#'
#' * `push(...)` - element or elements to stack, elements are converted to list
#' * `pop(n)` - removes and returns `n` latest elements from stack, if `n` is missing, remove all elements
#'
#' Following slots are available:
#'
#' * items - stack memory, a pre-allocated
#' * size - current size of the stack
#'
#' Modifying these slots could lead to inconsistent behaviour.
#'
#' @param init an initial size of the stack
#' @return an environment containing callable methods, see details
#'
#' @examples
#' s = new_stack()
#' s$push("foo", "bar", "baz")
#'
#' identical(s$pop(1), list("foo"))
#' identical(s$pop(), list("bar", "baz"))
#' identical(s$pop(), list())
#'
#' @export
new_stack = function(init = 20L){
    items = vector("list", init)
    size = 0L

    push = function(...){
        new = list(...)
        new_size = length(new) + size
        while(new_size > length(items))
            items[[length(items) + init]] = list(NULL)

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
#' These are handled in `test`, `test_set`, and `test_file` or `test_dir`.
#'
#' Methods:
#'
#' * add(x) -- append test result 'x' to the memory
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

    add = function(x){
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

    if(exists(".mutr", envir = globalenv(), mode = "environment")){
        env = globalenv()$.mutr
        env$add(res)

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


test_file = function(file){
    if(!exists(".mutr", envir = globalenv(), mode = "environment")){
            init_mutr("exit")
            on.exit(deinit_mutr(print = TRUE), add = TRUE)
            }

    source(file, chdir = TRUE)

    invisible()
    }


test_dir = function(dir){
    if(!exists(".mutr", envir = globalenv(), mode = "environment")){
                init_mutr("exit")
                on.exit(deinit_mutr(print = TRUE), add = TRUE)
                }
    files = dir(dir, "^test[^.]*\\.[rR]$", full.names = TRUE)
    lapply(files, test_file)

    invisible()
    }


#' Test if an object is an error
is_error = function(x){
    inherits(x, c("try-error", "error", "simpleError"))
    }


#' Negate usable in a pipe
not = function(x){
    !x
    }
