#' Package development functions
#'
#' These functions provide a dependency-light alternative to `devtools`.
#'
#' `test_pkg()` install and runs tests for specified package, these tests must be created by
#' some unit-testing framework in that they need to be able to be run outside of `R CMD check`.
#'
#' The `check()` maps to the `R CMD check` and `document()` runs the `roxygen2::roxygenize`
#' function to generate documentation.
#'
#' @param pkg a path to the package directory
#' @param as_cran run the `R CMD check` with the CRAN preset (internet connection required)
#' @return nothing, these functions are run for their side-effect
#'
#' @export
test_pkg = function(pkg = "."){
    pkg_name = pkg_name(pkg)

    # set local library
    tmp_lib = file.path(tempdir(), "r-lib")
    if(!dir.exists(tmp_lib)) dir.create(tmp_lib)
    .libPaths(c(tmp_lib, .libPaths()))

    # install pkg
    pkg_install(pkg, tmp_lib)

    # remove package from search path if its already there
    if(paste0("package:", pkg_name) %in% search())
        detach(paste0("package:", pkg_name), unload = TRUE, force = TRUE, character.only = TRUE)

    test_files = list.files(file.path(pkg, "tests"), full.names = TRUE)
    test_files = Filter(function(x) utils::file_test("-f", x), test_files)

    # run the code inside pkg environment
    env = new.env(parent = getNamespace(pkg_name))
    for(test_file in test_files){
        rm(list = ls(env, all.names = TRUE), envir = env)
        sys.source(test_file, chdir = TRUE, envir = env, toplevel.env = getNamespace(pkg_name))
        }
    }


#' @rdname test_pkg
#' @export
check = function(pkg = ".", as_cran = FALSE){
    tmp = tempdir()

    pkg_file = pkg_build(pkg)
    pkg_check(pkg_file, as_cran = as_cran)
    }


#' @rdname test_pkg
#' @export
document = function(pkg = "."){
    if(!requireNamespace("roxygen2"))
        stop("package roxygen2 required")

    roxygen2::roxygenize(pkg)
    }


#' Utility package functions
#'
#' These functions map to the `R CMD INSTALL`, `R CMD build` and `R CMD check` commands.
#'
#' The functions `pkg_install`, `pkg_build`, and `pkg_check` map to the respective `R CMD`
#' commands. By default, all output files are created in a temporary directory.
#' The `pkg_name` is an utility functions that reads the `DESCRIPTION` file and returns the
#' package name.
#'
#' @param pkg a path to package source (`pkg_install()`, `pkg_build()`, `pkg_name()`)
#' or tarball (`pkg_install()`, `pkg_check()`)
#' @param path a path where package is installed, build, or checked, defaults to `tempdir()`
#' @param as_cran whether to run `R CMD check` with the `--as-cran` preset
#' @return `pkg_install` and `pkg_check` do not have a return value,
#'         `pkg_build` returns a path to the compiled package tarball,
#'          `pkg_name` returns a name of the package in specified path
#'
#' @name pkg_cmd

#' @rdname pkg_cmd
#' @export
pkg_install = function(pkg, path = tempdir()){
    res = tools::Rcmd(c(
        "INSTALL",
        paste0("--library=", path),
        "--no-help",
        "--no-staged-install",
        pkg
        ))
    if(res != 0) stop("installation error", call. = FALSE)
    }


#' @rdname pkg_cmd
#' @export
pkg_build = function(pkg, path = tempdir()){
    res = tools::Rcmd(c("build", "--no-build-vignettes", "--no-manual", pkg))
    if(res != 0) stop("build error", call. = FALSE)

    pkg_file = dir(pkg, pattern = paste0(pkg_name(pkg), ".*\\.tar\\.gz"), full.names = TRUE)
    pkg_tmpfile = file.path(path, basename(pkg_file))

    if(pkg_file != pkg_tmpfile)
        file.rename(pkg_file, pkg_tmpfile)

    pkg_tmpfile
    }


#' @rdname pkg_cmd
#' @export
pkg_check = function(pkg, path = tempdir(), as_cran = FALSE){
    args = if(as_cran) "--as-cran" else c("--no-build-vignettes", "--no-manual")
    args = c("-o", path, args)
    tools::Rcmd(c("check", args, pkg))
    }


#' @rdname pkg_cmd
#' @export
pkg_name = function(pkg){
    descr = file.path(pkg, "DESCRIPTION")
    if(!(file.exists(descr) && utils::file_test("-f", descr)))
        stop("no description found, is ", pkg, " a path to a package?")
    read.dcf(descr, fields = "Package")[1] 
    }
