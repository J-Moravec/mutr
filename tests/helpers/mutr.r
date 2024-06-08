# mutr: minimal unit-testing framework
#
# a simplified copy-pastable version inspired by https://jera.com/techinfo/jtns/jtn002
#
# For a more feature-full version along with some goodies see the `mutr` package.
.TEST_FAIL = 0 
.TEST_TOTAL = 0
.TEST_SETS = 0

TEST_PRINT = function(){
    cat(
        "[",
        .TEST_SETS, "sets,",
        .TEST_TOTAL, "tests,",
        .TEST_FAIL, "failed,",
        .TEST_TOTAL - .TEST_FAIL, "passed",
        "]\n\n"
        )
    if(.TEST_FAIL != 0)
        stop("Some tests did not pass.\n", call. = FALSE)

    invisible()
    }


TEST = function(expr){
    res = try(expr, silent = TRUE)
    expr = paste0(deparse(substitute(expr)), collapse = "")

    .TEST_TOTAL <<- .TEST_TOTAL + 1

    if(!isTRUE(res)){
        .TEST_FAIL <<- .TEST_FAIL + 1
        cat("Error in ", expr, ": is not TRUE\n", sep = "")
        }

    invisible(res)
    }


TEST_SET = function(msg, expr){
    .TEST_SETS <<- .TEST_SETS + 1

    cat("  ", msg, "\n", sep = "")
    res = try(expr, silent = TRUE)

    invisible()
    }


TEST_FILE = function(file){
    sys.source(file, envir = environment(), chdir = TRUE)

    invisible()
    }


TEST_DIR = function(dir){
    files = dir(dir, "^test[^.]*\\.[rR]$", full.names = TRUE)
    lapply(files, TEST_FILE)

    invisible()
    }
