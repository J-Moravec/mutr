# test-mutr.r: tests for the unit-testing framework mutr

TEST__CHECK = function(fail, total, sets){
    env = get(".TESTS", envir = globalenv())
    stopifnot(env$FAIL == fail, env$TOTAL == total, env$SETS == sets)
    }

TEST__FAIL = function(expr, .msg = NULL, f = TEST, ...){
    .expr = substitute(expr)
    out = capture.output(eval(as.call(list(f, .expr, ...))))
    if(is.null(.msg)) .msg = paste0("  Error in ", deparse(substitute(expr)), ": is not TRUE!")
    if(!identical(out, .msg))
        stop("Output message differs from expected!\n",
             "  Output: ", out, "\n",
             "  Expected: ", .msg)
    }

TEST__PASS = function(expr, f = TEST, ...){
    .expr = substitute(expr)
    out = capture.output(eval(as.call(list(f, .expr, ...))))
    stopifnot(identical(out, character()))
    }

TEST__RESET = function(){
    rm(".TESTS", envir = globalenv())
    }

TEST_INIT()
TEST__CHECK(0, 0, 0)
TEST__PASS(TRUE)
TEST__CHECK(0, 1, 0)
TEST__FAIL(FALSE)
TEST__FAIL(FALSE, "  Error in FALSE: is not TRUE!")
TEST__FAIL(c(1:3), "  Error in c(1:3): is not TRUE!")
TEST__CHECK(3, 4, 0)

TEST_SET("", {
    TEST__PASS(TRUE)
    TEST__FAIL(FALSE)
    TEST__FAIL(NA)
    TEST__FAIL(1)
    })
TEST__CHECK(6, 8, 1)

TEST__PASS(stop("foo"), f = TEST_ERROR)
TEST__PASS(stop("foo"), f = TEST_ERROR, pattern = "foo")
TEST__CHECK(6, 10, 1)
TEST__FAIL(stop("foo"), f = TEST_ERROR, pattern = "bar",
           .msg = "  Error in stop(\"foo\"): does not signal specified error!")
TEST__FAIL("foo", f = TEST_ERROR,
           .msg = "  Error in \"foo\": does not signal specified error!")
TEST__CHECK(8, 12, 1)

TEST__PASS("foo", f = TEST_NOT_ERROR)
TEST__PASS(1, f = TEST_NOT_ERROR)
TEST__FAIL(stop("foo"), f = TEST_NOT_ERROR,
           .msg = "  Error in stop(\"foo\"): does signal an error!")
TEST__CHECK(9, 15, 1)

# Cleanup
TEST__RESET()
rm("TEST__CHECK", "TEST__FAIL", "TEST__PASS", "TEST__RESET")
