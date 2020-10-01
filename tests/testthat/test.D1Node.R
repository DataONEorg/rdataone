test_that("dataone library loads", {
  expect_true(require(dataone))
})

test_that("CNode ping", {
  skip_on_cran()
  library(dataone)
  # 'cnProd' is defined in 'helper-base.R' for all tests
  alive <- ping(cnProd)
  expect_true(alive)
})

test_that("CNode object index query works with query list param", {
  skip_on_cran()
  library(dataone)
  # Test query of CN object index using query string
  queryParams <- "q=id:doi*&rows=2&wt=xml"
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, cnProd))
  if (authValid) {
    if(getAuthMethod(am, cnProd) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  result <- query(cnProd, queryParams, as="list")
  #resultList <- parseSolrResult(result)
  expect_true(length(result) == 2)
  expect_match(result[[2]]$id, "doi:")
  size <- result[[1]]$size
  expect_s3_class(result[[1]]$dateUploaded, "POSIXct")
  expect_type(size, "double")

  # Test query of CN object index using query list
  queryParamList <- list(q="id:doi*", rows="5", fq="(abstract:chlorophyll AND dateUploaded:[2000-01-01T00:00:00Z TO NOW])", fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
  result <- query(cnProd, queryParamList, as="list")
  # The CN can return null if it is under heavy load.
  if(is.null(result) || length(result) == 0) skip("DataONE CN is busy")
  expect_match(result[[1]]$id, "doi:")
  size <- result[[1]]$size
  expect_type(result[[1]]$size, "double")
  expect_s3_class(result[[1]]$dateUploaded, "POSIXct")
  expect_match(result[[1]]$abstract, "chlorophyll", ignore.case=TRUE)
  
  # Test a query that contains embedded quotes
  queryParamList <- list(q="(attribute:lake) and (attribute:\"Percent Nitrogen\")", rows="10",
                        fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
  result <- query(cnProd, queryParamList, as="data.frame")
  expect_true(class(result) == "data.frame")
  expect_true(nrow(result) > 0)
  
  # Test if query can handle solr syntax error
  queryParamList <- list(q="(attribute:lake) and attribute:\"Percent Nitrogen\")", rows="10",
                         fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
  result <- query(cnProd, queryParamList, as="data.frame")
  expect_true(is.null(result))
  
  # Test if query can handle solr syntax error (mispelled field name "attr")
  queryParamList <- list(q="(attribute:lake) and attr:\"Percent Nitrogen\")", rows="10",
                         fl="title,id,abstract,size,dateUploaded,attributeName", wt="xml")
  result <- query(cnProd, queryParamList, as="data.frame")
  expect_true(is.null(result))
})

test_that("Object listing works for CNode, MNode", {
  skip_on_cran()
  library(dataone)
  
  # Note: this test assumes that there are at least 5 EML 2.1.0 documents in DataONE
  fromDate <- "2001-01-01T01:01:01.000+00:00"
  toDate <- "2015-12-31T01:01:01.000+00:00"
  formatId <- "eml://ecoinformatics.org/eml-2.1.0"
  start <- 0
  count <- 5
  objects <- listObjects(cnProd, fromDate=fromDate, toDate=toDate, formatId=formatId, start=start, count=count)
  # The XML parser used in listObjects creates one more element than returned elements, used to hold attributes?
  expect_equal(length(objects) - 1, count)
  for (i in 1:(length(objects)-1) ) {
    expect_match(objects[i]$objectInfo$formatId, formatId)
  }
  # Note: this test assumes that there are at least 5 EML 2.1.0 documents in KNB
  #mn <- getMNode(cnProd, "urn:node:KNB")
  objects <- listObjects(cnProd, fromDate=fromDate, toDate=toDate, formatId=formatId, start=start, count=count)
  # The XML parser used in listObjects creates one more element than returned elements, used to hold attributes?
  expect_equal(length(objects) - 1, count)
  for (i in 1:(length(objects)-1) ) {
    expect_match(objects[i]$objectInfo$formatId, formatId)
  }
  
  # Test invalid input
  fromDate <- "20-01-01T01:01:01.000+00:00" # Invalid year
  toDate <- "2015-12-31T01:01:01.000+00:00" # valid
  err <- try(objects <- listObjects(cnProd, fromDate=fromDate, toDate=toDate, formatId=formatId, start=start, count=count), silent=TRUE)
  expect_match(class(err), ("try-error"))
  
  fromDate <- "2013-01-01T01:01:01.000+00:00" # valid
  toDate <- "01/01/15" # Invalid - not ISO 8601
  try(objects <- listObjects(cnProd, fromDate=fromDate, toDate=toDate, formatId=formatId, start=start, count=count), silent=TRUE)
  expect_match(class(err), ("try-error"))

})

test_that("listQueryEngines, getQueryEngineDescription works for CNode, MNode", {
  skip_on_cran()
  library(dataone)
  
  # Get list of query engines for a CN, and get description for each engine
  engines <- listQueryEngines(cnProd)
  expect_gt(length(engines), 0)
  for (i in 1:length(engines)) {
    engineDesc <- getQueryEngineDescription(cnProd, engines[[i]])
    expect_gt(length(engineDesc), 0)
    expect_match(engineDesc$name, engines[[i]])
    expect_true(class(engineDesc$queryFields) == "data.frame")
  }
  
  # Get list of query engines for an MN, and get description for each engine
  mn <- getMNode(cnProd, "urn:node:KNB")
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
  skip_on_cran()
  library(dataone)
  
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, cnProd))
  if (authValid) {
    if(getAuthMethod(am, cnProd) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  # This test assumes that there are at least two pids on KNB that are DOIs (i.e. begin with "doi:")
  # This test requests two results and checks that 2 results are returned.
  queryParams <- "q=id:doi*&rows=2&wt=xml"
  result <- query(cnProd, queryParams, as="list")
  expect_true(length(result) == 2)
  expect_match(result[[2]]$id, "doi:")
  size <- result[[1]]$size
  expect_type(size, "double")
})

test_that("MNode object index query works", {
  skip_on_cran()
  library(dataone)
  queryParams <- "q=id:doi*&rows=2&wt=xml"
  #queryParams <- 'q=attribute:"net primary production" AND (abstract:"above ground" OR title:"above ground")'
  #mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
  #mn <- MNode(mn_uri)
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, mnKNB))
  if (authValid) {
    if(getAuthMethod(am, mnKNB) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  result <- query(mnKNB, queryParams, as="list")
  expect_true(length(result) > 0)
  pid <- result[[1]]$id
  expect_type(pid, "character")
  expect_match(pid, "doi:")
  # Inspect numeric value returned from Solr result type "long"
  size <- result[[1]]$size
  expect_type(size, "double")
  # Inspect list value converted from Solr result type "<arr>"
  attList <- result[[1]]$attribute
  if (!is.null(attList)) {
    expect_type(attList, "list")
    expect_type(attList[[1]], "character")
    expect_true(length(attList) > 0)
  }
  
  # Request that an XML object is returned
  result <- query(mnKNB, queryParams, as="xml", parse=TRUE)
  expect_s3_class(result, "XMLInternalDocument")
  
  # Request that a character object is returned
  result <- query(mnKNB, queryParams, as="xml", parse=FALSE)
  expect_type(result, "character")
  expect_match(result, "<?xml")
})

test_that("D1Node archive() works",{
  skip_on_cran()
  library(dataone)
  # Create a csv file for the data object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  #mnId <- "urn:node:mnStageUCSB2"
  #d1c <- new("D1Client", env="STAGING", mNodeid=mnId)
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
  if (authValid) {
    if(getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    # Set 'subject' to authentication subject, if available, so we will have permission to change this object
    subject <- getAuthSubject(am, d1cTest@mn)
    # If subject isn't available from the current authentication method, then get from DataONE
    if (is.na(subject) || subject == "public") {
      creds <- echoCredentials(d1cTest@cn)
      subject <- creds$person$subject
      if(is.null(subject) || is.na(subject)) skip("This test requires a valid DataONE user identity>\")")
    }
    
    do1 <- new("DataObject", format="text/csv", user=subject, filename=csvfile)
    # Set replication off, to prevent the bug of serialNumber increasing due to replication bug
    uploadDataObject(d1cTest, do1, replicate=FALSE, public=TRUE)
    id1 <- getIdentifier(do1)
    md1 <- getSystemMetadata(d1cTest@mn, id1)
    # Run the archive test if both metadata objects sync'd
    if (!is.null(md1)) {
      tstPid <- archive(d1cTest@mn, id1)
      expect_equal(tstPid, id1)
    }
    tstMd1 <- getSystemMetadata(d1cTest@mn, id1)
    expect_true(tstMd1@archived, info=sprintf("Pid %s was not archived properly", id1))
  } else {
      skip("This test requires valid authentication.")
  }
})

test_that("D1Node isAuthorized() works",{
  skip_on_cran()
  library(dataone)
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, cnProd))
  # Don't use a cert on Mac OS X
  if (authValid) {
    if(getAuthMethod(am, cnProd) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  # Send an authorization check to the D1 production CN.
  canRead <- isAuthorized(cnProd, "doi:10.6073/pasta/7fcb8fea57843fae65f63094472f502d", "read")
  expect_true(canRead)
  canWrite <- isAuthorized(cnProd, "doi:10.6073/pasta/7fcb8fea57843fae65f63094472f502d", "write")
  expect_false(canWrite)
  canChange <- isAuthorized(cnProd, "doi:10.6073/pasta/7fcb8fea57843fae65f63094472f502d", "changePermission")
  expect_false(canChange)
    
  # Now send a check to a member node.
  #mn <- getMNode(cnProd, "urn:node:KNB")
  canRead <- isAuthorized(mnKNB, "doi:10.6085/AA/pisco_recruitment.149.1", "read")
  expect_true(canRead)
  canWrite <- isAuthorized(mnKNB, "doi:10.6085/AA/pisco_recruitment.149.1", "write")
  expect_false(canWrite)
  canChange <- isAuthorized(mnKNB, "doi:10.6085/AA/pisco_recruitment.149.1", "changePermission")
  expect_false(canChange)
})

