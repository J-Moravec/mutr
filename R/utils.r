test_pkg = function(pkg){
    # Build the pkg and load it locally
    tmp_lib = file.path(tempdir(), "r-lib")
    if(!dir.exists(tmp_lib)) dir.create(tmp_lib)

    .libPaths(c(tmp_lib, .libPaths()))
    pkg_name = read.dcf(file.path(pkg, "DESCRIPTION"), fields = "Package")[1] 

    res = tools::Rcmd(c(
        "INSTALL",
        paste0("--library=", tmp_lib),
        "--no-help",
        "--no-staged-install",
        pkg
        ))

    if(res != 0) stop("installation error", call. = FALSE)

    if(paste0("package:", pkg_name) %in% search())
        detach(paste0("package:", pkg_name), unload = TRUE, force = TRUE, character.only = TRUE)

    test_dir = file.path(pkg, "tests")
    test_files = list.files(test_dir, full.names = TRUE) |>
        Filter( f = function(x) file_test("-f", x))

    env = new.env(parent = getNamespace(pkg_name))
    for(test_file in test_files){
        rm(list = ls(env, all.names = TRUE), envir = env)
        sys.source(test_file, chdir = TRUE, envir = env, toplevel.env = getNamespace(pkg_name))
        }
    }


check_pkg = function(pkg){
    tmp = tempdir()


    }


restart_session = function(cmd = commandArgs()[1], wd = getwd(), f = NULL){

    }
