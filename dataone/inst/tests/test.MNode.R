context("MNode tests")
test_that("dataone library loads", {
	library(dataone)
})
test_that("MNode constructors", {
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
	mn <- MNode(mn_uri)
	expect_that(mn@endpoint, matches(mn_uri))
})
test_that("MNode getCapabilities()", {
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
	mn <- MNode(mn_uri)
    xml <- getCapabilities(mn)
	val <- xmlName(xmlRoot(xml))
	expect_that(val, matches("node"))
	expect_that(mn@identifier, matches("urn:node"))
})
test_that("MNode get()", {
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
	expect_that(cname, matches("data.frame"))
    cn <- CNode()
    knb <- getMNode(cn, "urn:node:KNB")
	pid <- "doi:10.5063/F1QN64NZ"
	xml <- get(mn, pid)
	cname <- class(xml)[1]
	expect_that(cname, matches("XML"))
})
test_that("MNode getSystemMetadata()", {
    library(dataone)
    cn <- CNode()
    mn <- getMNode(cn, "urn:node:KNB")
    pid <- "doi:10.5063/F1QN64NZ"
    xml <- getSystemMetadata(mn, pid)
    cname <- class(xml)[1]
    expect_that(cname, matches("XML"))
})
test_that("MNode generateIdentifier()", {
    library(dataone)
    cn <- CNode("DEV")
    mn <- getMNode(cn, "urn:node:mnDemo5")
    newid <- generateIdentifier(mn, "UUID")
    cname <- class(newid)
    expect_that(cname, matches("character"))
    expect_that(newid, matches("urn:uuid:"))
})
test_that("MNode describe()", {
  library(dataone)
  mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v1"
  mn <- MNode(mn_uri)
  res <- describe(mn, "knb.473.1")
  expect_is(res, "list")
  expect_equal(res$`content-type`, "text/xml")
})
