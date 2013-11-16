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
	val <- xmlName(xmlRoot(xml))
	expect_that(val, matches("node"))
	expect_that(mn@identifier, matches("urn:node"))
})
test_that("MNode get() works", {
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
	mn <- MNode(mn_uri)
    pid <- "doi:10.5063/F1QN64NZ"
    xml <- get(mn, pid)
	cname <- class(xml)[1]
	expect_that(cname, matches("XML"))
    pid <- "solson.5.1"
    obj <- get(mn, pid)
	cname <- class(obj)[1]
	expect_that(cname, matches("character"))
})
