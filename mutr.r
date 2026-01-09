# mutr: minimal unit-testing framework
# inspired by https://jera.com/techinfo/jtns/jtn002
# version: 0.0.4
# https://github.com/J-Moravec/mutr


# Helper functions
MUTR_PARTIAL = function(.f, ...){
    new = eval(substitute(alist(...)))
    old = formals(.f)
    formals(.f) = modifyList(old, new)
    .f
    }


is.error = function(x){
    inherits(x, c("try-error" ,"error"))
    }


not = function(x){
    !x
    }


TEST_INIT = function(indent = "  ", print_level = 0){
    FAIL = 0
    TOTAL = 0
    SETS = 0
    UNIT = FALSE
    LEVEL = 0
    INDENT = indent
    PRINT_LEVEL = print_level
    MESSAGE = vector("character", 64)
    rm(indent, print_level)

    CONTEXT = function(msg = ""){
        if(is.null(msg) || msg == "") return(invisible())
        msg = paste0(strrep(INDENT, LEVEL), msg, "\n")
        if(PRINT_LEVEL >= LEVEL) cat(msg, sep = "") else MESSAGE[LEVEL + 1] <<- msg
        }

    PRINT = function(){
        cat(MESSAGE[1:(LEVEL + 1)], sep = "")
        MESSAGE[1:(LEVEL + 1)] <<- ""
        }

    assign(".TESTS", environment(), envir = globalenv())
    }


SET_CONTEXT = function(msg="", add = FALSE){
    env = get(".TESTS", envir = globalenv())
    env$CONTEXT(msg)
    if(add) env$LEVEL = env$LEVEL + 1
    }


ADD_CONTEXT = MUTR_PARTIAL(SET_CONTEXT, add = TRUE)


TEST_PRINT = function(){
    env = get(".TESTS", envir = globalenv())

    cat(
        "[",
        env$SETS, "sets,",
        env$TOTAL, "tests,",
        env$FAIL, "failed,",
        env$TOTAL - env$FAIL, "passed",
        "]\n\n"
        )
    if(env$FAIL != 0)
        stop("Some tests did not pass.\n", call. = FALSE)

    invisible()
    }


TEST = function(expr, msg = "is not TRUE!", call = NULL, test = TRUE){
    if(is.null(call)) call = paste0(deparse(substitute(expr)), collapse = "")
    res = try(expr, silent = TRUE)
    env = get(".TESTS", envir = globalenv())

    if(test)
        env$TOTAL = env$TOTAL + 1

    if(!isTRUE(res)){
        if(test){
            env$FAIL = env$FAIL + 1
            } else {
            env$UNIT = TRUE
            }

        env$PRINT()
        indent = strrep(env$INDENT, env$LEVEL)
        cat(indent, "Error in ", call, ": ", msg, "\n", sep = "")
        }

    invisible(res)
    }


TEST_CONTEXT = function(msg, expr, unit = FALSE, set = FALSE){
    env = get(".TESTS", envir = globalenv())
    env$CONTEXT(msg)

    if(set) env$SETS = env$SETS + 1

    if(unit){
        env$TOTAL = env$TOTAL + 1
        env$UNIT = FALSE
        }

    env$LEVEL = {level = env$LEVEL} + 1
    eval(expr)
    env$LEVEL = level

    if(env$UNIT) env$FAIL = env$FAIL + 1

    invisible()
    }


TEST_SET = MUTR_PARTIAL(TEST_CONTEXT, set = TRUE)
TEST_UNIT = MUTR_PARTIAL(TEST_CONTEXT, unit = TRUE)


TEST_FILE = function(file){
    env = get(".TESTS", envir = globalenv())
    sys.source(file, env = environment(), chdir = TRUE)
    env$LEVEL = 0
    invisible()
    }


TEST_DIR = function(dir){
    files = dir(dir, "^test[^.]*\\.[rR]$", full.names = TRUE)
    lapply(files, TEST_FILE)

    invisible()
    }


TEST_ERROR = function(
    expr, msg = "does not signal specified error!", pattern = "", call = NULL, test = TRUE){
    if(is.null(call)) call = paste0(deparse(substitute(expr)), collapse = "")
    e = tryCatch(expr, error = \(e) e)
    (is.error(e) && grepl(pattern, conditionMessage(e))) |>
        TEST(call = call, msg = msg, test = test)
    }


TEST_NOT_ERROR = function(expr, msg = "does signal an error!", call = NULL, test = TRUE){
    if(is.null(call)) call = paste0(deparse(substitute(expr)), collapse = "")
    e = tryCatch(expr, error = \(e) e)
    is.error(e) |> not() |> TEST(call = call, msg = msg, test = test)
    }


ASSERT = MUTR_PARTIAL(TEST, test = FALSE)
ASSERT_ERROR = MUTR_PARTIAL(TEST_ERROR, test = FALSE)
ASSERT_NOT_ERROR = MUTR_PARTIAL(TEST_NOT_ERROR, test = FALSE)
