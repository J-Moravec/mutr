# mutr.r
#
# Minimal test framework
# inspired by https://jera.com/techinfo/jtns/jtn002 and https://github.com/siu/minunit

#' Create new stack object
#'
#' Create a new stack object with reference semantics with two methods
#' `push` adds objects to the stack and `pop` removes them.
#'
#' This is dependency-free class based on function closures with reference semantics.
#' Internally, the stack is represented as a pre-allocated `list` that is extended as required.
#' Initially, the size of the `list` is set to the `init`, and each time it needs to be extended
#' it is extended by the `init` value as well.
#'
#' The `pop(n)` method removes `n` latest items added to stack. When `n` is not specified,
#' all items are removed and the stack is emptied.
#'
#' @param init an initial size of the stack
#' @return an environment containing callable methods using the `env$method()` semantics
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


new_mutr = function(print = c("test", "set", "exit")){
    sets = 0
    tests = 0
    failed = 0
    passed = 0
    set_failed = FALSE
    print = match.arg(print)
    messages = new_stack()

    add = function(x){
        tests <<- tests + 1L

        if(isTRUE(x)){
            passed <<- passed + 1L
            } else {
            failed <<- failed + 1L
            set_failed <<- TRUE
            }
        }

    add_set = function(){
        sets <<- sets + 1L
        set_failed <<- FALSE
        }

    structure(environment(), "class" = "counter")
    }


init_mutr = function(print){
    mutr = new_mutr(print)
    env = globalenv()
    env$.mutr = mutr

    invisible()
    }


deinit_mutr = function(print = FALSE){
    env = globalenv()$.mutr
    rm(".mutr", envir = globalenv())

    if(print){
        cat(
            "[",
            env$sets, "sets,",
            env$tests, "tests:",
            env$failed, "failed,",
            env$passed, "passed",
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

    res = try(expr, silent = TRUE)


    cat("  ", msg, ": ", sep = "")
    if(class(res)[1] == "try-error" || env$set_failed) cat("FAIL\n") else cat("PASS\n")

    if(env$print == "set" && env$set_failed) cat(unlist(env$messages$pop()))

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
