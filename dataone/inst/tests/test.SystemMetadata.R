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
  
#  sysmeta <- SystemMetadata(xmlRoot(xml))
#  expect_that(sysmeta@identifier, matches(testid))
#  expect_that(sysmeta@archived, is_true())
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
    #print(xml)
    expect_that(xml, matches("<d1:systemMetadata"))
})
