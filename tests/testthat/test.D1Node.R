context("D1Node tests")
test_that("dataone library loads", {
  library(dataone)
})

test_that("CNode ping", {
  library(dataone)
  cn <- CNode("STAGING")
  alive <- ping(cn)
  expect_true(alive)
})

test_that("CNode object index query works with query list param", {
  library(dataone)
  # Test query of CN object index using query string
  queryParams <- "q=id:doi*&rows=2&wt=xml"
  cn <- CNode("STAGING")  
  am <- AuthenticationManager()
  suppressMessages(authValid <- isAuthValid(am, cn))
  if (authValid) {
    if(getAuthMethod(am, cn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  result <- query(cn, queryParams, as="list")
  #resultList <- parseSolrResult(result)
  expect_true(length(result) == 2)
  expect_match(result[[2]]$id, "doi:")
  size <- result[[1]]$size
  expect_is(size, "numeric")

  # Test query of CN object index using query list
  queryParamList <- list(q="id:doi*", rows="5", fq="(abstract:chlorophyll AND dateUploaded:[2000-01-01T00:00:00Z TO NOW])", fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
  result <- query(cn, queryParamList, as="list")
  expect_true(length(result) > 0)
  expect_match(result[[1]]$id, "doi:")
  size <- result[[1]]$size
  expect_is(result[[1]]$size, "numeric")
  expect_match(result[[1]]$abstract, "chlorophyll")
  
  # Test a query that contains embedded quotes
  queryParamList <- list(q="(attribute:lake) and (attribute:\"Percent Nitrogen\")", rows="1000",
                        fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
  result <- query(cn, queryParamList, as="data.frame")
  expect_true(class(result) == "data.frame")
  expect_true(nrow(result) > 0)
  
  # Test if query can handle solr syntax error
  queryParamList <- list(q="(attribute:lake) and attribute:\"Percent Nitrogen\")", rows="1000",
                         fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
  result <- query(cn, queryParamList, as="data.frame")
  expect_true(is.null(result))
  
  # Test if query can handle solr syntax error (mispelled field name "attr")
  queryParamList <- list(q="(attribute:lake) and attr:\"Percent Nitrogen\")", rows="1000",
                         fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
  result <- query(cn, queryParamList, as="data.frame")
  expect_true(is.null(result))
})

test_that("Object listing works for CNode, MNode", {
  library(dataone)
  
  cn <- CNode("STAGING")
  fromDate <- "2013-01-01T01:01:01.000+00:00"
  toDate <- "2015-12-31T01:01:01.000+00:00"
  formatId <- "eml://ecoinformatics.org/eml-2.1.0"
  start <- 0
  count <- 5
  objects <- listObjects(cn, fromDate=fromDate, toDate=toDate, formatId=formatId, start=start, count=count)
  # The XML parser used in listObjects creates one more element than returned elements, used to hold attributes?
  expect_equal(length(objects) - 1, count)
  for (i in 1:(length(objects)-1) ) {
    expect_match(objects[i]$objectInfo$formatId, formatId)
  }
  mn <- getMNode(cn, "urn:node:mnStageUCSB2")
  objects <- listObjects(cn, fromDate=fromDate, toDate=toDate, formatId=formatId, start=start, count=count)
  # The XML parser used in listObjects creates one more element than returned elements, used to hold attributes?
  expect_equal(length(objects) - 1, count)
  for (i in 1:(length(objects)-1) ) {
    expect_match(objects[i]$objectInfo$formatId, formatId)
  }
  
  # Test invalid input
  fromDate <- "20-01-01T01:01:01.000+00:00" # Invalid year
  toDate <- "2015-12-31T01:01:01.000+00:00" # valid
  err <- try(objects <- listObjects(cn, fromDate=fromDate, toDate=toDate, formatId=formatId, start=start, count=count), silent=TRUE)
  expect_that(class(err), (matches("try-error")))
  
  fromDate <- "2013-01-01T01:01:01.000+00:00" # valid
  toDate <- "01/01/15" # Invalid - not ISO 8601
  try(objects <- listObjects(cn, fromDate=fromDate, toDate=toDate, formatId=formatId, start=start, count=count), silent=TRUE)
  expect_that(class(err), (matches("try-error")))

})

test_that("listQueryEngines, getQueryEngineDescription works for CNode, MNode", {
  library(dataone)
  
  #cn <- CNode("STAGING2")
  # Get list of query engines for a CN, and get description for each engine
  cn <- CNode("STAGING2")
  engines <- listQueryEngines(cn)
  expect_gt(length(engines), 0)
  for (i in 1:length(engines)) {
    engineDesc <- getQueryEngineDescription(cn, engines[[i]])
    expect_gt(length(engineDesc), 0)
    expect_match(engineDesc$name, engines[[i]])
    expect_true(class(engineDesc$queryFields) == "data.frame")
  }
  
  # Get list of query engines for an MN, and get description for each engine
  mn <- getMNode(cn, "urn:node:mnTestKNB")
  engines <- listQueryEngines(mn)
  expect_gt(length(engines), 0)
  for (i in 1:length(engines)) {
    if(engines[[i]] == "pathquery") next
    engineDesc <- getQueryEngineDescription(mn, engines[[i]])
    expect_gt(length(engineDesc), 0)
    expect_match(engineDesc$name, engines[[i]])
    expect_true(class(engineDesc$queryFields) == "data.frame")
  }
})

test_that("CNode object index query works with query string param", {
  library(dataone)
  
  cn <- CNode("STAGING2")
  am <- AuthenticationManager()
  suppressMessages(authValid <- isAuthValid(am, cn))
  if (authValid) {
    if(getAuthMethod(am, cn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  queryParams <- "q=id:doi*&rows=2&wt=xml"
  result <- query(cn, queryParams, as="list")
  expect_true(length(result) == 2)
  expect_match(result[[2]]$id, "doi:")
  size <- result[[1]]$size
  expect_is(size, "numeric")
})

test_that("MNode object index query works", {
  library(dataone)
  queryParams <- "q=id:doi*&rows=2&wt=xml"
  #queryParams <- 'q=attribute:"net primary production" AND (abstract:"above ground" OR title:"above ground")'
  #mn_uri <- "https://dev.nceas.ucsb.edu/knb/d1/mn/v1"
  mn_uri <- "https://mn-stage-ucsb-2.test.dataone.org/knb/d1/mn/v1"
  mn <- MNode(mn_uri)
  am <- AuthenticationManager()
  suppressMessages(authValid <- isAuthValid(am, mn))
  if (authValid) {
    if(getAuthMethod(am, mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
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
  result <- query(mn, queryParams, as="xml", parse=TRUE)
  expect_is(result, "XMLInternalDocument")
  
  # Request that a character object is returned
  result <- query(mn, queryParams, as="xml", parse=FALSE)
  expect_is(result, "character")
  expect_match(result, "<?xml")
})

test_that("D1Node archive() works",{
  skip_on_cran()
  library(dataone)
  # Create a csv file for the data object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  mnId <- "urn:node:mnStageUCSB2"
  d1c <- new("D1Client", env="STAGING", mNodeid=mnId)
  am <- AuthenticationManager()
  suppressMessages(authValid <- isAuthValid(am, d1c@mn))
  if (authValid) {
    if(getAuthMethod(am, d1c@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    # Set 'subject' to authentication subject, if available, so we will have permission to change this object
    subject <- getAuthSubject(am, d1c@mn)
    # If subject isn't available from the current authentication method, then get from DataONE
    if (is.na(subject) || subject == "public") {
      creds <- echoCredentials(d1c@cn)
      subject <- creds$person$subject
      if(is.null(subject) || is.na(subject)) skip("This test requires a valid DataONE user identity>\")")
    }
    
    do1 <- new("DataObject", format="text/csv", user=subject, mnNodeId=mnId, filename=csvfile)
    # Set replication off, to prevent the bug of serialNumber increasing due to replication bug
    uploadDataObject(d1c, do1, replicate=FALSE, public=TRUE)
    id1 <- getIdentifier(do1)
    md1 <- getSystemMetadata(d1c@mn, id1)
    # Run the archive test if both metadata objects sync'd
    if (!is.null(md1)) {
      tstPid <- archive(d1c@mn, id1)
      expect_equal(tstPid, id1)
    }
    tstMd1 <- getSystemMetadata(d1c@mn, id1)
    expect_true(tstMd1@archived, info=sprintf("Pid %s was not archived properly", id1))
  } else {
      skip("This test requires valid authentication.")
  }
})
