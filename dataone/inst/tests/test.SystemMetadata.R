context("SystemMetadata tests")
test_that("dataone library loads", {
	library(dataone)
})
test_that("SystemMetadata constructors", {
	library(dataone)
	sysmeta <- SystemMetadata()
	expect_that(sysmeta@serialVersion, equals(1))
})
#test_that("MNode getCapabilities()", {
#	library(dataone)
#	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
#	mn <- MNode(mn_uri)
#    xml <- getCapabilities(mn)
#	val <- xmlName(xmlRoot(xml))
#	expect_that(val, matches("node"))
#	expect_that(mn@identifier, matches("urn:node"))
#})