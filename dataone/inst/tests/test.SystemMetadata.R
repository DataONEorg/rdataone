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
  doc <- xmlParseDoc("../testfiles/sysmeta.xml", asText=FALSE)
  expect_that(xmlValue(xmlRoot(doc)[["identifier"]]), matches(testid))
  xml <- xmlRoot(doc)
  #getEncoding(doc)
  sysmeta <- parseSystemMetadata(sysmeta, xmlRoot(xml))
  expect_that(sysmeta@identifier, matches(testid))
  expect_that(sysmeta@archived, is_true())
  csattrs <- xmlAttrs(xml[["checksum"]])
  expect_that(sysmeta@checksumAlgorithm, matches(csattrs[[1]]))
  expect_that(grep("urn:node:KNB", sysmeta@preferredNodes) > 0, is_true())
  expect_that(grep("urn:node:mnUNM1", sysmeta@preferredNodes) > 0, is_true())
  expect_that(grep("urn:node:BADNODE", sysmeta@blockedNodes) > 0, is_true())
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
    #cat(xml)
    expect_that(xml, matches("<d1:systemMetadata"))
    expect_that(xml, matches("<preferredMemberNode>urn:node:KNB</preferredMemberNode>"))
    expect_that(xml, matches("<blockedMemberNode>urn:node:BADNODE</blockedMemberNode>"))
    # TODO: check document validity
    # TODO: check tree equivalence with original XML document
})
