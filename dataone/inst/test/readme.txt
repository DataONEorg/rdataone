RUnit testing
=============

RUnit tests offer another way to test an R extension's functionality, aside from
what can be done through R CMD CHECK.  The main advantages to RUnit testing are 
being able to run tests outside a build cycle, and flexibility in organizing and
running the tests.

In this package, tests have been divided into 'unit' tests, which by and large do
not depend on any outside infrastructure, and 'integration' tests, which do rely
to varying degrees, on elements outside the package itself.

All test files conform to the standard of having the prefix 'runit', and all 
test functions have the prefix 'test'.  This allows the use of the RUnit
methods runTestFile, and defineTestSuite using default parameters for locating
the tests.

Running one Test File
---------------------
1. from terminal shell, navigate to d1_client_r/inst/test directory, or any 
   subdirectory where the tests you want to run are located.  This makes it easier
   later to specify the files.
2. start an R session
3. load RUnit and dataone:
 
    library(RUnit)
    library(dataone)
    
4. execute the desired test files. For example:

    runTestFile("unit/runit.CertificateManager.R")

Running one Test
-----------------
Is the same as running one test file, but providing a more specific pattern
for locating the test functions.  For example:

    runTestfile("unit/runit.CertificateManager.R", testFuncRegexp = "^test.showClientCert.*")
    
    
Running one of the test suites
-------------------------------
From any directory in the d1_client_r project:

source the file  "dataone/inst/test/runUnitTests.R" or 
source the file  "dataone/inst/test/runIntegrationTests.R"

(if outside the project, it will try to get the test dir from the R installation library)
 