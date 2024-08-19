# mutr: minimal unit-testing framework
#
# a simplified copy-pastable version inspired by https://jera.com/techinfo/jtns/jtn002

TEST_INIT = function(){
    env = new.env(parent = emptyenv())
    env$FAIL = 0
    env$TOTAL = 0
    env$SETS = 0

    assign(".TESTS", env, envir = globalenv())
    }


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


TEST = function(expr, msg = "is not TRUE!", call = NULL){
    call = if(is.null(call)) deparse(substitute(expr)) |> paste0(collapse = "")
    res = try(expr, silent = TRUE)

    env = get(".TESTS", envir = globalenv())
    env$TOTAL = env$TOTAL + 1

    if(!isTRUE(res)){
        env$FAIL = env$FAIL + 1
        cat("  Error in ", call, ": ", msg, "\n", sep = "")
        }

    invisible(res)
    }


TEST_SET = function(msg, expr){
    env = get(".TESTS", envir = globalenv())
    env$SETS = env$SETS + 1

    cat("", msg, "\n", sep = "")
    eval(expr)

    invisible()
    }


TEST_FILE = function(file){
    sys.source(file, env = environment(), chdir = TRUE)
    invisible()
    }


TEST_DIR = function(dir){
    files = dir(dir, "^test[^.]*\\.[rR]$", full.names = TRUE)
    lapply(files, TEST_FILE)

    invisible()
    }


is.error = function(x){
    inherits(x, c("try-error" ,"error"))
    }


not = function(x){
    !x
    }


TEST_ERROR = function(expr, msg = "does not signal required error!", pattern = "", call = NULL){
    call = if(is.null(call)) deparse(substitute(expr)) |> paste0(collapse = "")
    e = tryCatch(expr, error = \(e) e)
    (is.error(e) && grepl(pattern, conditionMessage(e))) |> TEST(call = call, msg = msg)
    }
