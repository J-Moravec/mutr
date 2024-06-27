# mutr -- Minimal Unit-Testing in R

`mutr` is a minimal unit-testing framework in R.
It is inspired by [https://jera.com/techinfo/jtns/jtn002] and [https://github.com/siu/minunit].

The package `mutr` contains one implementation of this unit-testing framework, while a second simplified implementation is used for self-testing, and can be copied to provide dependency-free unit-testing.

On top of this, the package `mutr` offers simplified development tools akin to `devtools` package, with `test_pkg`, `check`, and `document` (`roxygen2` is still required for the latter).
