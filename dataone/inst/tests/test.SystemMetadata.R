context("SystemMetadata tests")
test_that("dataone library loads", {
	library(dataone)
})
test_that("SystemMetadata constructors", {
	library(dataone)
	sysmeta <- SystemMetadata()
	expect_that(sysmeta@serialVersion, equals(1))
})
test_that("XML SystemMetadata parsing works", {
  library(dataone)
  testid <- "doi:10.xxyy/AA/tesdoc123456789"
  sysmeta <- SystemMetadata()
  expect_that(sysmeta@serialVersion, equals(1))
  xml <- xmlParseDoc("../testfiles/sysmeta.xml", asText=FALSE)
  expect_that(xmlValue(xmlRoot(xml)[["identifier"]]), matches(testid))
  newsysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
  expect_that(newsysmeta@identifier, matches(testid))
  expect_that(newsysmeta@archived, is_true())
})
