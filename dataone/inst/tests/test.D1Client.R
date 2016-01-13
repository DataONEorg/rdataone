context("D1Client tests")
test_that("dataone library loads", {
	library(dataone)
})
test_that("D1Client constructors", {
        library(dataone)
        cli <- new("D1Client")
        expect_that(cli, not(is_null()))
        expect_that(class(cli), matches("D1Client"))
        expect_that(cli@cn@baseURL, matches ("https://cn.dataone.org/cn"))
        
        cli <- new("D1Client", env="PROD", mNodeid="urn:node:KNB")
        expect_that(cli, not(is_null()))
        expect_that(class(cli), matches("D1Client"))
        expect_that(cli@cn@baseURL, matches ("https://cn.dataone.org/cn"))
        expect_that(cli@mn@baseURL, matches ("https://knb.ecoinformatics.org/knb/d1/mn"))
        
        cn <- CNode("STAGING2")
        cli <- new("D1Client", cn=cn, mn=getMNode(cn, "urn:node:mnTestKNB"))
        expect_that(cli, not(is_null()))
        expect_that(class(cli), matches("D1Client"))
        expect_that(cli@cn@baseURL, matches ("https://cn.stage-2.test.dataone.org/cn"))
        expect_that(cli@mn@baseURL, matches ("https://dev.nceas.ucsb.edu/knb/d1/mn"))
        
        cli <- D1Client()
        expect_that(cli, not(is_null()))
        expect_that(class(cli), matches("D1Client"))
        expect_that(cli@cn@baseURL, matches ("https://cn.dataone.org/cn"))
        
        cli <- D1Client("STAGING")
        expect_that(cli, not(is_null()))
        expect_that(class(cli), matches("D1Client"))
        expect_that(cli@cn@baseURL, matches ("https://cn-stage.test.dataone.org/cn"))
        
        cli <- D1Client("SANDBOX")
        expect_that(cli, not(is_null()))
        expect_that(class(cli), matches("D1Client"))
        expect_that(cli@cn@baseURL, matches ("https://cn-sandbox.test.dataone.org/cn"))
        
        cli <- D1Client("DEV")
        expect_that(cli, not(is_null()))
        expect_that(class(cli), matches("D1Client"))
        expect_that(cli@cn@baseURL, matches("https://cn-dev.test.dataone.org/cn"))
})

test_that("D1Client methods", {
  skip_on_cran()
  # Test getEndPoint()
  cli <- D1Client("DEV")
  cnUrl <- getEndpoint(cli)
  expect_match(cnUrl, "https://cn-dev.test.dataone.org/cn")
  # Test getMNodeId()
  cli <- D1Client("STAGING2", "urn:node:mnTestKNB")
  expect_match(getMNodeId(cli), "urn:node:mnTestKNB")
  # Test setMNodeId
  cli <- new("D1Client", env="SANDBOX2")
  cli <- setMNodeId(cli, "urn:node:mnDemo2")
  expect_match(cli@mn@identifier, "urn:node:mnDemo2")
  
  # Test getMN (deprecated)
  suppressWarnings(testMN <- getMN(cli))
  expect_match(testMN@identifier, "urn:node:mnDemo2")
  suppressWarnings(testMN <- getMN(cli, nodeid="urn:node:mnDemo2"))
  expect_match(testMN@identifier, "urn:node:mnDemo2")
  
  # Test getCN (deprecated)
  suppressWarnings(testCN <- getCN(cli))
  expect_match(testCN@baseURL, "test.dataone")
  
  # Test listMemberNodes
  cli <- D1Client("PROD")
  nodes <- listMemberNodes(cli)
  expect_more_than(length(nodes), 0)
  expect_identical(class(nodes), "list")
  
})

test_that("D1Client getDataObject", {
    library(dataone)
    library(digest)
    cli <- D1Client("PROD", "urn:node:KNB")
    expect_that(cli, not(is_null()))
    expect_that(class(cli), matches("D1Client"))
    expect_that(cli@cn@baseURL, matches ("https://cn.dataone.org/cn"))
    am <- AuthenticationManager()
    suppressWarnings(authValid <- isAuthValid(am, cli@mn))
    if(authValid) {
      # Skip if Mac OS and X.509 Certificate
      if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
      
      # Try retrieving a known object from the PROD environment
      pid <- "solson.5.1"
      obj <- getDataObject(cli, pid)
      cname <- class(obj)[1]
      expect_that(cname, matches("DataObject"))
      expect_that(class(obj@sysmeta), matches("SystemMetadata"))
      expect_that(getIdentifier(obj), matches(pid))
      expect_that(getFormatId(obj), matches("text/csv"))
      data <- getData(obj)
      sha1 <- digest(data, algo="md5", serialize=FALSE, file=FALSE)
      expect_that(sha1, matches(obj@sysmeta@checksum))
    }
})

test_that("D1Client uploadDataPackage works", {
  skip_on_cran()
  library(dataone)
  library(datapackage)
  # Create a csv file for the science object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  d1c <- D1Client(env="STAGING", mNodeid="urn:node:mnStageUCSB2")
  #d1c <- D1Client(env="SANDBOX2", mNodeid="urn:node:mnDemo2")
  #d1c <- D1Client(env="DEV2", mNodeid="urn:node:mnDevUCSB2")
  expect_false(is.null(d1c))
  #preferredNodes <- c("urn:node:mnDemo9")
  preferredNodes <- NA
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, d1c@mn)
  options(warn = warnLevel)
  if (authValid) {
    if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    
    dp <- new("DataPackage")
    # Create DataObject for the science data 
    sciObj <- new("DataObject", format="text/csv", mnNodeId=getMNodeId(d1c), filename=csvfile)
    # It's possible to set access rules for DataObject now, or for all DataObjects when they are uploaded to DataONE via uploadDataPackage
    expect_that(sciObj@sysmeta@identifier, matches("urn:uuid"))
    sciObj <- setPublicAccess(sciObj)
    accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", "uid=slaughter,o=unaffiliated,dc=example,dc=org"), permission=c("write", "changePermission"))
    sciObj <- addAccessRule(sciObj, accessRules)
    addData(dp, sciObj)
    expect_true(is.element(sciObj@sysmeta@identifier, getIdentifiers(dp)))
    
    # Create metadata object that describes science data
    emlFile <- system.file("extdata/sample-eml.xml", package="dataone")
    metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", mnNodeId=getMNodeId(d1c), filename=emlFile)
    expect_that(metadataObj@sysmeta@identifier, matches("urn:uuid"))
    addData(dp, metadataObj)
    expect_true(is.element(metadataObj@sysmeta@identifier, getIdentifiers(dp)))
    
    # Associate the metadata object with the science object it describes
    insertRelationship(dp, subjectID=getIdentifier(metadataObj), objectIDs=getIdentifier(sciObj))
    
    # Upload the data package to DataONE    
    resourceMapId <- uploadDataPackage(d1c, dp, replicate=TRUE, numberReplicas=1, preferredNodes=preferredNodes,  public=TRUE, accessRules=accessRules)
    expect_true(!is.null(resourceMapId))
    
    # Now test if the package members can be uploaded a second time. uploadDataObject should test the sysmeta@dataUploaded of each object
    # and not let it be uploaded again.
    ids <- getIdentifiers(dp)
    for(idInd in 1:length(ids)) {
      thisId <- ids[idInd]
      thisObj <- getMember(dp, thisId)
      testId <- uploadDataObject(d1c, thisObj)
      expect_null(testId)
    }
  } else {
      skip("This test requires valid authentication.")
  }
})

test_that("D1Client createD1Object works", {
  skip_on_cran()
  library(dataone)
  library(datapackage)
  # Create a csv file for the science object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  d1c <- D1Client(env="STAGING", mNodeid="urn:node:mnStageUCSB2")
  #d1c <- D1Client(env="SANDBOX2", mNodeid="urn:node:mnDemo2")
  #d1c <- D1Client(env="DEV2", mNodeid="urn:node:mnDevUCSB2")
  expect_false(is.null(d1c))
  #preferredNodes <- c("urn:node:mnDemo9")
  preferredNodes <- NA
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  suppressWarnings(authValid <- isAuthValid(am, d1c@mn))
  if (authValid) {
    if(getAuthMethod(am) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    # Create D1Object for the science data 
    suppressWarnings(sciObj <- new("D1Object", format="text/csv", mnNodeId=getMNodeId(d1c), filename=csvfile))
    # It's possible to set access rules for DataObject now, or for all DataObjects when they are uploaded to DataONE via uploadDataPackage
    expect_that(sciObj@dataObject@sysmeta@identifier, matches("urn:uuid"))
    sciObj <- setPublicAccess(sciObj)
    # Upload the data object to DataONE
    sucess <- createD1Object(d1c, sciObj)
    expect_true(success)
    # Now see if we can download the object from DataONE
  } else {
    skip("This test requires valid authentication.")
  }
})