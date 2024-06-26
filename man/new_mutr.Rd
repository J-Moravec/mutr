% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutr.r
\name{new_mutr}
\alias{new_mutr}
\alias{init_mutr}
\alias{deinit_mutr}
\title{Create, initialize, and deinitialize mutr object}
\usage{
new_mutr(print = c("test", "set", "exit"))

init_mutr(print)

deinit_mutr(print = FALSE)
}
\arguments{
\item{print}{when to print errors, see details}
}
\value{
`new_mutr` returns an environment containing callable methods,
        `init_mutr` and `deinit_mutr` are run for side-effects.
}
\description{
Create, initialize, and deinitialize a mutr object.
}
\details{
This objects stores information required to track tests, test sets, and their error messages.

`new_mutr` creates a new object, `init_mutr` saves it and `deinit_mutr` removes it from the
user's global environment. Before the removal, `deinit_mutr` also prints test results.

The `print` argument specifies when the errors are being printed,
whether after each `test`, `test_set`, on exit during de-initialization.
These are handled in `test`, `test_set`,`test_file`, and `test_dir`.

Methods:

* add_test(x) -- append test result 'x' to the memory
* add_set() -- increase the set ounter by one

Slots:

* sets -- the total number of test sets, see `[test_set]`
* tests -- the total number of tests
* failed -- the number of failed tests
* print -- when to print errors, see details
}
