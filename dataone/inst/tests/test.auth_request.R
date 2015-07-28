context("auth_request tests")
test_that("dataone library loads", {
	library(dataone)
})

test_that("auth_get", {
  library(dataone)
  uri <- "https://cn.dataone.org/cn/v1/formats"
  format_list <- content(auth_get(uri))
  cname <- class(format_list)[1]
  expect_that(cname, matches("XML"))
  xml <- XML::saveXML(format_list)
  expect_that(grep("METADATA", xml) > 0, is_true())
  expect_that(grep("Ecological Metadata", xml) > 0, is_true())
})
