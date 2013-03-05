
##
##  Runs all of the tests in the test/unit directory,
##  trying first to find the tests in the project directory, then
##  trying to locate in the R installed packages library
##

library(dataone)
library(RUnit)

testDir <- function() {
    curDir <- Sys.getenv("PWD")
    if (grepl("d1_client_r",curDir)) {
        ## in the project directory, so have the path
        testDir <- sub("d1_client_r.+","d1_client_r/dataone/inst/test/unit",curDir)
    } else {
        testDir <- file.path(.path.package(package="dataone"),"inst/test/unit")
    }
    return(testDir)
}

theTestSuite <- defineTestSuite("unitTests",
        dirs = testDir(),
        testFileRegexp = "^runit.+\\.R")

testResult <- runTestSuite(theTestSuite)
printTextProtocol(testResult)