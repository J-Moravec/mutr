test_pkg = function(pkg = "."){
    pkg_name = pkg_name(pkg)

    # set local library
    tmp_lib = file.path(tempdir(), "r-lib")
    if(!dir.exists(tmp_lib)) dir.create(tmp_lib)
    .libPaths(c(tmp_lib, .libPaths()))

    # install pkg
    res = tools::Rcmd(c(
        "INSTALL",
        paste0("--library=", tmp_lib),
        "--no-help",
        "--no-staged-install",
        pkg
        ))
    if(res != 0) stop("installation error", call. = FALSE)

    # remove package from search path if its already there
    if(paste0("package:", pkg_name) %in% search())
        detach(paste0("package:", pkg_name), unload = TRUE, force = TRUE, character.only = TRUE)

    test_files = list.files(file.path(pkg, "tests"), full.names = TRUE)
    test_files = Filter(function(x) file_test("-f", x), test_files)

    # run the code inside pkg environment
    env = new.env(parent = getNamespace(pkg_name))
    for(test_file in test_files){
        rm(list = ls(env, all.names = TRUE), envir = env)
        sys.source(test_file, chdir = TRUE, envir = env, toplevel.env = getNamespace(pkg_name))
        }
    }


check_pkg = function(pkg = ".", as_cran = FALSE){
    tmp = tempdir()

    pkg_file = pkg_build(pkg)
    pkg_check(pkg_file, as_cran)
    }


document_pkg = function(pkg = "."){
    if(!requireNamespace("roxygen2"))
        stop("package roxygen2 required")

    roxygen2::roxygenize(pkg)
    }


pkg_build = function(pkg, path = tempdir()){
    res = tools::Rcmd(c("build", "--no-build-vignettes", "--no-manual", pkg))
    if(res != 0) stop("build error", call. = FALSE)

    pkg_file = dir(pkg, pattern = paste0(pkg_name(pkg), ".*\\.tar\\.gz"), full.names = TRUE)
    pkg_tmpfile = file.path(path, basename(pkg_file))

    if(pkg_file != pkg_tmpfile)
        file.rename(pkg_file, pkg_tmpfile)

    pkg_tmpfile
    }


pkg_check = function(pkg, as_cran = FALSE){
    args = if(as_cran) "--as-cran" else c("--no-build-vignettes", "--no-manual")
    tools::Rcmd(c("check", args, pkg))
    }


pkg_name = function(path){
    descr = file.path(path, "DESCRIPTION")
    if(!(file.exists(descr) && file_test("-f", descr)))
        stop("no description found, is ", pkg, " a path to a package?")
    read.dcf(descr, fields = "Package")[1] 
    }
