context("auth_request tests")
test_that("dataone library loads", {
	library(dataone)
})

test_that("auth_get", {
  library(dataone)
  library(httr)
  library(XML)
  uri <- "https://cn.dataone.org/cn/v2/formats"
  format_list <- xmlParse(content(dataone:::auth_get(uri), as="text"))
  cname <- class(format_list)[1]
  expect_that(cname, matches("XML"))
  xml <- XML::saveXML(format_list)
  expect_that(grep("METADATA", xml) > 0, is_true())
  expect_that(grep("Ecological Metadata", xml) > 0, is_true())
})
