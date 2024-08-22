set_script_dir = function(){
    dir = commandArgs(FALSE) |>
        grep(pattern = "^--file=", value = TRUE) |>
        getElement(1) |>
        sub(pattern = "^--file=", replacement = "") |>
        dirname()
    setwd(dir)
    }

# Run from the script's directory when run using Rscript
if(sys.nframe() == 0) set_script_dir()


source("helpers/mutr.r")
source("helpers/test-mutr.r")

TEST_INIT()
TEST_DIR("tests")
TEST_PRINT()
