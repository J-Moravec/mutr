#options(warn = 2) # convert warnings to errors

#set_script_dir = function(){
#    commandArgs(FALSE)
#    dir = commandArgs(FALSE) |>
#        grep(pattern="^--file=", value=TRUE) |>
#        sub(pattern="^--file=", replacement="") |>
#        dirname()
#    setwd(dir)
#    }

#if(sys.nframe() == 0) set_script_dir()

#source("../mutr.r")


test_set(msg = "Test basic functionality", {
    test(TRUE)
    test(TRUE)
    test({Sys.sleep(1); TRUE})
    })

test_set(msg = "Test advanced functionality", {
    test(TRUE)
    test({Sys.sleep(1); TRUE})
    })

test_set(msg = "Test throwing error", {
    test(FALSE)
    })
