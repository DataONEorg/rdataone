context("MNode tests")
test_that("dataone library loads", {
	library(dataone)
})
test_that("MNode constructors work", {
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
	mn <- MNode(mn_uri)
	expect_that(mn@endpoint, matches(mn_uri))
})
test_that("MNode getCapabilities() works", {
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
	mn <- MNode(mn_uri)
    xml <- getCapabilities(mn)
	expect_that(xml$headers$status, matches("200"))
	expect_that(xml$headers$`content-type`, matches("text/xml"))
})
