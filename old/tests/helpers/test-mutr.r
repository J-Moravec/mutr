# Test for unit-tests
#
# This will test a simplified version of mutr used for self-testing

# helpers
TEST_CHECK = function(fail, total, sets){
    stopifnot(.TEST_FAIL == fail, .TEST_TOTAL == total, .TEST_SETS == sets)
    }

TEST_FAIL = function(expr){
    msg = capture.output(TEST(expr))
    stopifnot("Error in expr: is not TRUE" == msg)
    }

TEST_PASS = function(expr){
    msg = capture.output(TEST(expr))
    stopifnot(msg == character(0))
    }

TEST_RESET = function(){
    .TEST_FAIL <<- 0
    .TEST_TOTAL <<- 0
    .TEST_SETS <<- 0
    }

TEST_CHECK(0, 0, 0)
TEST_PASS(TRUE)
TEST_CHECK(0, 1, 0)
TEST_FAIL(FALSE)
TEST_CHECK(1, 2, 0)
TEST_SET("", {
    TEST_PASS(TRUE)
    TEST_FAIL(FALSE)
    TEST_FAIL(NA)
    TEST_FAIL(1)
    })
TEST_CHECK(4, 6, 1)


# Cleanup
TEST_RESET()
rm("TEST_CHECK", "TEST_FAIL", "TEST_PASS", "TEST_RESET")
