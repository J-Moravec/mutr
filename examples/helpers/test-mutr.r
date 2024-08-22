# Test for unit-tests
#
# This will test a simplified version of mutr used for self-testing

# helpers
TEST__CHECK = function(fail, total, sets){
    env = get(".TESTS", envir = globalenv())
    stopifnot(env$FAIL == fail, env$TOTAL == total, env$SETS == sets)
    }

TEST__FAIL = function(expr){
    msg = capture.output(TEST(expr))
    stopifnot("  Error in expr: is not TRUE!" == msg)
    }

TEST__PASS = function(expr){
    msg = capture.output(TEST(expr))
    stopifnot(msg == character(0))
    }

TEST__RESET = function(){
    rm(".TESTS", envir = globalenv())
    }

TEST_INIT()
TEST__CHECK(0, 0, 0)
TEST__PASS(TRUE)
TEST__CHECK(0, 1, 0)
TEST__FAIL(FALSE)
TEST__CHECK(1, 2, 0)
TEST_SET("", {
    TEST__PASS(TRUE)
    TEST__FAIL(FALSE)
    TEST__FAIL(NA)
    TEST__FAIL(1)
    })
TEST__CHECK(4, 6, 1)


# Cleanup
TEST__RESET()
rm("TEST__CHECK", "TEST__FAIL", "TEST__PASS", "TEST__RESET")
