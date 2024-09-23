# mutr -- Minimal Unit-Testing in R

`mutr` is a minimal copyable unit-testing framework in R inspired by [https://jera.com/techinfo/jtns/jtn002] and [https://github.com/siu/minunit].

`mutr` is designed to be minimal, but with a reasonable set of features.
If you want something a bit more feature full, consider [testthat](https://testthat.r-lib.org/) or [tinytest](https://github.com/markvanderloo/tinytest).

## Installation

Simply copy-paste it into your package or next to your code, no dependencies beside R are required.

## Usage

To run the examples, simply navigate to the `mutr` folder and type `Rscript examples/run_all.r`

### Framework

To use `mutr`, you need to set-up your test framework.

An example of a `mutr` setup as well as a folder structure can be found in the **examples** folder.

I like to create a single point of entry `test_all.r` containing the following:

```{r}
source("helpers/mutr.r")
source(helpers/test-mutr.r")

TEST_INIT()
TEST_DIR("tests")
TEST_PRINT()
```

First, we `source` the `mutr` framework as well as unit-tests for the `mutr` framework so we test while we test.

Follows a series of command that initializes (`TEST_INIT()`), runs all tests in the `tests` folder (`TEST_DIR("tests")`), and finally prints the status (`TEST_PRINT()`).

Helper functions that should be applicable to all tests can be also placed here, or rather, in the `helpers` folder and sourced here.

The `test_all.r` also contains a peculiar piece of code that will set the workdir to the directory where `test_all.r` is placed

### Tests

The core of `mutr` are three functions:

* `TEST(expr)`
* `TEST_SET(msg, expr)`
* `TEST_DIR(dir)`

`TEST(expr)` is the basic building block, it evaluates the `expr` and prints an error message if the expression does not evaluate to `TRUE`. It  also increments the test and error counters if required. The `TEST(expr)` is equivalent to testthat's `expect_true()`.

`TEST_SET(msg, expr)` is logical grouping of tests under a label `msg`. It simply prints the `msg` and evalutes the `expr` which can contain any helper code and a few `TEST()` calls. It also increments the set counter.

Finally, on the highest level there is `TEST_DIR(dir)`, it simply runs all files in a chosen directory that start with `test`. All files are run in their local path (i.e., path of the file, not path of the `TEST_DIR`) and own environment to minimize side-effects, although some side-effects cannot be entirely prevented.

For instance, consider a following example:

```{r}
TEST_SET("myfun returns TRUE", {
    # any code can be placed here
    myfun = function(){TRUE}

    # passing
    TEST(myfun())
    TEST(myfun() == TRUE)
    TEST(identical(myfun(), TRUE))

    # fail, print error, increment error counter
    # but do not stop the framework
    TEST(!myfun())
    TEST(myfun() == FALSE)
    TEST(identical(myfun(), FALSE))
    })

```

If you would like to derive specific `TEST` expressions to test other outcomes beside `TRUE`, see the the `TEST_ERROR` in `mutr.r`.

## Why

I will give you three reasons, because three is magical number:

`mutr` is:

* minimal -- just the thing you might need, nothing more, nothing less
* dependency free -- no need to install anything new
* copy-pastable -- copy it where you want and it will work and stay with you forever

### Minimalism

I started like you, imported bunch of packages to just do my work, even if it meant importing a single package (and all its dependencies) to just solve a relatively simple problem once.

In time, I changed and became quite a minimalist, partially because I had to write a code on some very old machines, or code that should be easy to use by other people in the future.

This meant writing as simple code as possible and with as little dependencies as possible.

### Why write this when testthat and tinytests are a thing?

Both packages are amazing, `testthat` was the first unit-testing framework I used with an amazing documentation. The trio of `devtools`, `testthat`, and `roxygen2` integrate well and are a great framework around which you can build a package.

But I had previously quite an akward time with `testthat` when it changed dependency from `crayon` to `cli`. Suddenly, `testthat` stopped working due to a bug in `cli`. Due to the layers of abstractions, the error was hard to parse, and when I finally found out, it turns out it was already fixed, but the update was not pushed to CRAN for almost a year.

`tinytest` is a much simpler alternative to `testthat` with no dependencies, and I need to start integrate it into my packages. But it lacks the superb documentation of `testthat` and I haven't got into it yet.

But both packages means adding a dependency, a developer dependency only, true, but a dependency.
If the package changes tomorrow, the code would have to by updated.

On the other hand, `mutr` will stay as it is. If it does job for you, no need to change it. New developers do not need to download anything new. If the `mutr` code will change tomorrow, code bundled with your package or analysis will stay constant.

### Why not a package?

`mutr` evolved from something I needed to quickly test some local code in an analysis, to a more complex package that tracked errors, to a simplified framework to said package, to the simplified framework itself.

`mutr` doesn't need to be package. It is just a few lines of code, the packaging framework would perhaps double its size and making its usage more cumbersome, as well as hide implementation details. Making a package from it would just hinder its potential.

`mutr` can be just copied where you want it. Changed as you need it, adapted to your personal needs. As with the original inspiration, unit-testing framework doesn't have to be complex to do what you need. Just fork it, or just copy and personalize it, ship it with your code.

## Examples:

I have been using `mutr` for some non-package projects to make sure that they are bug-free.
In fact, after a proof of concept was written, refactoring was done through a test-driven development. Being able to quickly setup testing and write tests made this quite productive.

* [rargs](https://github.com/J-Moravec/rargs) -- POSIX compatible copy-pastable argument parser
* [ncbi.r](https://github.com/J-Moravec/ncbi) -- Download reference genomes from NCBI
