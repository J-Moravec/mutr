set_script_dir = function(){
    file = grep(commandArgs(FALSE), pattern = "^--file=", value = TRUE)[1]
    dir = dirname(sub(file, pattern = "^--file=", replacement = ""))
    setwd(dir)
    }

# Run from the script's directory when run using Rscript
if(sys.nframe() == 0) set_script_dir()


source("helpers/mutr.r")
source("helpers/test-mutr.r")

TEST_INIT()
TEST_DIR("tests")
TEST_PRINT()
