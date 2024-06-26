% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutr.r
\name{test_context}
\alias{test_context}
\alias{test_set}
\alias{test_file}
\alias{test_dir}
\title{Test context}
\usage{
test_set(msg, expr)

test_file(file)

test_dir(dir)
}
\arguments{
\item{msg}{a name of the set}

\item{expr}{an expression containing `test`s, since only the `mutr::test` functions append
to the `.mutr` object, any other expressions won't influence the test counter.}

\item{file}{a file containing `test_set`s and `test`s}

\item{dir}{a directory containing files starting with `test`}
}
\description{
Run the `test` functions in a context, such as set, file, or a directory.
}
\details{
These function establish context for tests so that a structured test-suite can be constructed.
Each of the context functions `test_set`, `test_file`, and `test_dir` initialize the `.mutr`
object if it doesn't already exists, and create a `on.exit` destructor to print the test
result.
}
