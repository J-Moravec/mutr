# mutr 0.0.3

* added `ASSERT`, a variant of `TEST` that doesn't increase test count
* multiple asserts can be grouped within a single `TEST_UNIT`
* `TEST_UNIT` prints its own message if one or more `ASSERT` calls fail
* also added `ASSERT_ERROR` and `ASSERT_NOT_ERROR` variants of `TEST_ERROR` and `TEST_NOT_ERROR`

# mutr 0.0.2

* added `TEST_NOT_ERROR` to check that the expression doesn't throw error
* fixed incorrect assignment of `call` within `TEST` and `TEST_ERROR`
* enhanced self-testing `TEST__PASS` and `TEST__FAIL` to preserve the expression
* `TEST__PASS` and `TEST__FAIL` now can accept optional arguments
* added more self-tests.
* added versioning and `NEWS.md`
* removed old files (some functionality now in the [mpd](https://github.com/J-Moravec/mpd/) package
* a simple makefile for self-testing and to copy `mutr.r` into examples

# mutr 0.0.1

* initial version
