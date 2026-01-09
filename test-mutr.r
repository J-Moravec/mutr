# test-mutr.r: tests for the unit-testing framework mutr
# version: 0.0.4
# https://github.com/J-Moravec/mutr

## Helper functions
TEST__CHECK = function(fail, total, sets){
    env = get(".TESTS", envir = globalenv())
    stopifnot(env$FAIL == fail, env$TOTAL == total, env$SETS == sets)
    }


TEST__FAIL = function(expr, .msg = NULL, f = TEST, indent = "", ...){
    .expr = substitute(expr)
    out = capture.output(eval(as.call(list(f, .expr, ...))))
    if(is.null(.msg))
        .msg = paste0(indent, "Error in ", deparse(substitute(expr)), ": is not TRUE!")
    if(!identical(out, .msg))
        stop("Output message differs from expected!\n",
             "  Output: ", out, "\n",
             "  Expected: ", .msg)
    }


TEST__PASS = function(expr, f = TEST, .msg=character(), ...){
    .expr = substitute(expr)
    out = capture.output(eval(as.call(list(f, .expr, ...))))
    if(!identical(out, .msg))
        stop("Output message differs from expected!\n",
            "  output: ", out, "\n",
            "  Expected: ", .msg)
    }


TEST__RESET = function(init = TRUE){
    rm(".TESTS", envir = globalenv())
    if(init) TEST_INIT()
    }


## Check basics: TEST, ASSERT and helper functions
TEST_INIT()
TEST__CHECK(0, 0, 0)
TEST__PASS(TRUE)
TEST__CHECK(0, 1, 0)
TEST__FAIL(FALSE)
TEST__CHECK(1, 2, 0)
TEST__FAIL(FALSE, "Error in FALSE: is not TRUE!")
TEST__CHECK(2, 3, 0)
TEST__FAIL(c(1:3))
TEST__CHECK(3, 4, 0)
TEST__FAIL(c(1:3), "Error in c(1:3): is not TRUE!")
TEST__CHECK(4, 5, 0)
TEST__PASS(TRUE, f = ASSERT)
TEST__FAIL(FALSE, f = ASSERT)
TEST__CHECK(4, 5, 0) # ASSERT doesn't increase the test count
TEST__RESET()

## Check TEST_SET with TEST type functions
# TEST_SET groups TEST type functions
TEST_SET(NULL, {
    TEST__PASS(TRUE)
    TEST__FAIL(FALSE, indent = "  ")
    TEST__FAIL(NA, indent = "  ")
    TEST__FAIL(1, indent = "  ")
    })
TEST__FAIL(1) # indent is reset
TEST__CHECK(4, 5, 1)
TEST__RESET()

## Check TEST_UNIT with ASSERT type functions
TEST_UNIT(NULL, {
    TEST__PASS(TRUE, f = ASSERT)
    })
TEST__CHECK(0, 1, 0)
TEST__RESET()

TEST_UNIT(NULL, {
    TEST__PASS(TRUE, f = ASSERT)
    TEST__FAIL(FALSE, f = ASSERT, indent = "  ")
    })
TEST__CHECK(1, 1, 0)
TEST__RESET()

## Check TEST_UNIT output messages
TEST__PASS(ASSERT(TRUE), f = TEST_UNIT, msg = "")
TEST__PASS(ASSERT(TRUE), f = TEST_UNIT, msg = "test ASSERT", .msg = "test ASSERT")
TEST__FAIL(
    ASSERT(FALSE),
    f = TEST_UNIT, msg = "test ASSERT",
    .msg = c("test ASSERT", "  Error in FALSE: is not TRUE!")
    )
TEST__CHECK(1, 3, 0)
TEST__RESET()


## Check derived functions: TEST_ERROR, TEST_NOT_ERROR
TEST__PASS(stop("foo"), f = TEST_ERROR)
TEST__PASS(stop("foo"), f = TEST_ERROR, pattern = "foo")
TEST__CHECK(0, 2, 0)
TEST__FAIL(stop("foo"), f = TEST_ERROR, pattern = "bar",
           .msg = "Error in stop(\"foo\"): does not signal specified error!")
TEST__FAIL("foo", f = TEST_ERROR,
           .msg = "Error in \"foo\": does not signal specified error!")
TEST__CHECK(2, 4, 0)
TEST__RESET()

TEST__PASS("foo", f = TEST_NOT_ERROR)
TEST__PASS(1, f = TEST_NOT_ERROR)
TEST__FAIL(stop("foo"), f = TEST_NOT_ERROR,
           .msg = "Error in stop(\"foo\"): does signal an error!")
TEST__CHECK(1, 3, 0)
TEST__RESET()

## Check derived functions: ASSERT_ERROR, ASSERT_NOT_ERROR
TEST__PASS(stop("foo"), f = ASSERT_ERROR)
TEST__PASS(stop("foo"), f = ASSERT_ERROR, pattern = "foo")
TEST__FAIL("foo", f = ASSERT_ERROR,
           .msg = "Error in \"foo\": does not signal specified error!")
TEST__PASS("foo", f = ASSERT_NOT_ERROR)
TEST__FAIL(stop("foo"), f = ASSERT_NOT_ERROR,
           .msg = "Error in stop(\"foo\"): does signal an error!")
TEST__CHECK(0, 0, 0)

# Cleanup
TEST__RESET(init = FALSE)
rm("TEST__CHECK", "TEST__FAIL", "TEST__PASS", "TEST__RESET")
