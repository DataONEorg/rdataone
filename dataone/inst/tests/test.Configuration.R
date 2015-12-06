
context("Session configuration tests")
test_that("dataone library loads", {
  library(dataone)
})

test_that("Can read and write configuration parameters", {
  # Skip on CRAN as the session configuration calls write to ~/.dataone
  skip_on_cran()
  library(uuid)
  uuidTag <- UUIDgenerate()
  # Check that tests have been setup
  expect_that(class(uuidTag), equals("character"))
  config <- new("SessionConfig")  
  
  # Shouldn't be able to read config params unless we are in an active recording session
  err <- try(readConfig(config), silent=TRUE)
  expect_that(class(err), (matches("try-error")))
  
  # Read the sample configuration that is stored with the installed package. Parameters 
  # will be placed in the ".recordrConfig" environment
  loadConfig(config)
  expect_that(length(base::ls(".D1Config")), is_more_than(0))
  # Check that we can change a configuration value
  val1 <- getConfig(config, name="public_read_allowed")
  setConfig(config, "public_read_allowed", !val1)
  val2 <- getConfig(config, name="public_read_allowed")
  expect_that(val1 != val2, is_true())
  
  err <- try(setConfig(config, "public_read_allowed", 1), silent=TRUE)
  expect_that(class(err), (matches("try-error")))
  
  # Check that we can write out a configuration
  configFile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".R")
  saveConfig(config, file=configFile)
  expect_that(file.exists(configFile), is_true())
  found <- grep("public_read_allowed", readLines(configFile))
  expect_that(found, is_more_than(0))
  
  # Check that we can read in a stored configuration
  uuidTag <- UUIDgenerate()
  val3 <- getConfig(config, name="public_read_allowed")
  expect_that(val2 == val3, is_true())
  #unlink(configFile)
  
  unloadConfig(config)
  #expect_equal(length(base::ls(".D1Config")), 0)
})
