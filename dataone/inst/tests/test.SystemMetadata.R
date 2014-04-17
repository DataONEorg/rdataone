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
  sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
  expect_that(sysmeta@identifier, matches(testid))
  expect_that(sysmeta@archived, is_true())
  csattrs <- xmlAttrs(xmlRoot(xml)[["checksum"]])
  expect_that(sysmeta@checksumAlgorithm, matches(csattrs[[1]]))
})

test_that("XML SystemMetadata serialization works", {
    library(dataone)
    testid <- "doi:10.xxyy/AA/tesdoc123456789"
    sysmeta <- SystemMetadata()
    expect_that(sysmeta@serialVersion, equals(1))
    xml <- xmlParseDoc("../testfiles/sysmeta.xml", asText=FALSE)
    expect_that(xmlValue(xmlRoot(xml)[["identifier"]]), matches(testid))
    sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
    expect_that(sysmeta@identifier, matches(testid))
    expect_that(sysmeta@archived, is_true())
    xml <- serialize(sysmeta)
    cat(xml)
    expect_that(xml, matches("<d1:systemMetadata"))
    # TODO: check document validity
    # TODO: check tree equivalence with original XML document
})
