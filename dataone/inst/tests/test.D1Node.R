context("D1Node tests")
test_that("dataone library loads", {
  library(dataone)
})
test_that("CNode object index query works", {
  skip_on_cran()
  library(dataone)
  # Test query of CN object index using query string
  queryParams <- "q=id:doi*&rows=2&wt=xml"
  cn <- CNode("SANDBOX")
  result <- query(cn, queryParams, as="list")
  #resultList <- parseSolrResult(result)
  expect_true(length(result) == 2)
  expect_match(result[[2]]$id, "doi:")
  size <- result[[1]]$size
  expect_is(size, "numeric")
  
  # Test query of CN object index using query list
  queryParamList <- list(q="id:doi*", rows="5", fq="(abstract:chlorophyll AND dateUploaded:[2000-01-01T00:00:00Z TO NOW])", fl="title,id,abstract,size,dateUploaded", wt="xml")
  result <- query(cn, queryParamList, as="list")
  expect_true(length(result) > 0)
  expect_match(result[[1]]$id, "doi:")
  size <- result[[1]]$size
  expect_is(result[[1]]$size, "numeric")
  expect_match(result[[1]]$abstract, "chlorophyll")
})
test_that("MNode object index query works", {
  skip_on_cran()
  library(dataone)
  queryParams <- "q=id:doi*&rows=2&wt=xml"
  #mn_uri <- "https://dev.nceas.ucsb.edu/knb/d1/mn/v1"
  mn_uri <- "https://mn-stage-ucsb-2.test.dataone.org/knb/d1/mn/v1"
  mn <- MNode(mn_uri)
  result <- query(mn, queryParams, as="list")
  #expect_is(result, "XMLInternalDocument")
  #resultList <- parseSolrResult(result)
  expect_true(length(result) > 0)
  pid <- result[[1]]$id
  expect_is(pid, "character")
  expect_match(pid, "doi:")
  # Inspect numeric value returned from Solr result type "long"
  size <- result[[1]]$size
  expect_is(size, "numeric")
  # Inspect list value converted from Solr result type "<arr>"
  attList <- result[[1]]$attribute
  if (!is.null(attList)) {
    expect_is(attList, "list")
    expect_is(attList[[1]], "character")
    expect_true(length(attList) > 0)
  }
  
  # Request that an XML object is returned
  result <- query(mn, queryParams, as="parsed")
  expect_is(result, "XMLInternalDocument")
  
  # Request that a character object is returned
  result <- query(mn, queryParams, as="text")
  expect_is(result, "character")
  expect_match(result, "<?xml")
  
})