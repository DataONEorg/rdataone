context("D1Client tests")
test_that("dataone library loads", {
	expect_true(require(dataone))
})
test_that("D1Client constructors", {
        library(dataone)
        #cli <- new("D1Client")
        expect_false(is.null(d1cProd))
        expect_match(class(d1cProd), "D1Client")
        expect_match(d1cProd@cn@baseURL, "https://cn.dataone.org/cn")
        
        #cli <- new("D1Client", env="PROD", mNodeid="urn:node:KNB")
        expect_false(is.null(d1cKNB))
        expect_match(class(d1cKNB), "D1Client")
        expect_match(d1cKNB@cn@baseURL, "https://cn.dataone.org/cn")
        expect_match(d1cKNB@mn@baseURL, "https://knb.ecoinformatics.org/knb/d1/mn")
        
        # Skip the remainder of the tests because these test environments are 
        # often down due to upgrades, reconfiguring, testing new features.
        skip_on_cran()
        cli <- new("D1Client", cn=cnStaging2, mn=getMNode(cnStaging2, "urn:node:mnTestKNB"))
        expect_false(is.null(cli))
        expect_match(class(cli), "D1Client")
        expect_match(cli@cn@baseURL, "https://cn.stage-2.test.dataone.org/cn")
        expect_match(cli@mn@baseURL, "https://dev.nceas.ucsb.edu/knb/d1/mn")
        
        cli <- D1Client()
        expect_false(is.null(cli))
        expect_match(class(cli), "D1Client")
        expect_match(cli@cn@baseURL, "https://cn.dataone.org/cn")
        
        cli <- D1Client("STAGING")
        expect_false(is.null(cli))
        expect_match(class(cli), "D1Client")
        expect_match(cli@cn@baseURL, "https://cn-stage.test.dataone.org/cn")
        
        # Skip the hightly unstable environments when testing on cran
        skip_on_cran()
        cli <- D1Client("SANDBOX")
        expect_false(is.null(cli))
        expect_match(class(cli), "D1Client")
        expect_match(cli@cn@baseURL, "https://cn-sandbox.test.dataone.org/cn")
        
        #cli <- D1Client("DEV")
        #expect_false(is.null(cli))
        #expect_match(class(cli), "D1Client")
        #expect_match(cli@cn@baseURL, "https://cn-dev.test.dataone.org/cn")
})

test_that("D1Client methods", {  
  # Test listMemberNodes
  #cli <- D1Client("PROD")
  nodes <- listMemberNodes(d1cProd)
  expect_gt(length(nodes), 0)
  expect_identical(class(nodes), "list")
    
  # The remainder of this test uses development machines.
  skip_on_cran()
  # Test getEndPoint()
  cli <- D1Client("STAGING")
  cnUrl <- getEndpoint(cli)
  expect_match(cnUrl, "https://cn-stage.test.dataone.org/cn")
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
  
})

test_that("D1Client getDataObject", {
    library(dataone)
    library(digest)
    #cli <- D1Client("PROD", "urn:node:KNB")
    expect_false(is.null(d1cKNB))
    expect_match(class(d1cKNB), "D1Client")
    expect_match(d1cKNB@cn@baseURL, "https://cn.dataone.org/cn")
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, d1cKNB@mn))
    if(authValid) {
      # Skip if Mac OS and X.509 Certificate
      if(dataone:::getAuthMethod(am, d1cKNB@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    }
      
      # Try retrieving a known object from the PROD environment
    pid <- "solson.5.1"
    obj <- getDataObject(d1cKNB, pid)
    cname <- class(obj)[1]
    expect_match(cname, "DataObject")
    expect_match(class(obj@sysmeta), "SystemMetadata")
    expect_match(getIdentifier(obj), pid)
    expect_match(getFormatId(obj), "text/csv")
    data <- getData(obj)
    sha1 <- digest(data, algo="md5", serialize=FALSE, file=FALSE)
    expect_match(sha1, obj@sysmeta@checksum)
})

test_that("D1Client uploadDataObject with raw data works", {
  skip_on_cran()
  library(dataone)
  library(datapack)

  # Create a DataObject with a raw R object and upload to DataONE
  data <- charToRaw("1,2,3\n4,5,6\n")
  #d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
  expect_false(is.null(d1cTest))
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    # Create DataObject for the science data 
    do <- new("DataObject", format="text/csv", dataobj=data, mnNodeId=getMNodeId(d1cTest))
    expect_match(do@sysmeta@identifier, "urn:uuid")
    newId <- uploadDataObject(d1cTest, do, replicate=FALSE, preferredNodes=NA, public=TRUE)
    expect_true(!is.null(newId))
  } else {
    skip("This test requires valid authentication.")
  }
})

test_that("D1Client uploadDataObject with filename works", {
  skip_on_cran()
  library(dataone)
  library(datapack)
  
  # Create a csv file for the science object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  #d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
  expect_false(is.null(d1cTest))
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    # Create DataObject for the science data 
    do <- new("DataObject", format="text/csv", mnNodeId=getMNodeId(d1cTest), filename=csvfile)
    expect_match(do@sysmeta@identifier, "urn:uuid")
    newId <- uploadDataObject(d1cTest, do, replicate=FALSE, preferredNodes=NA ,  public=TRUE)
    expect_true(!is.null(newId))
  } else {
    skip("This test requires valid authentication.")
  }
})

test_that("D1Client uploadDataPackage works", {
  skip_on_cran()
  library(dataone)
  library(datapack)
  # Create a csv file for the science object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  #d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
  #d1c <- D1Client("SANDBOX2", "urn:node:mnDemo2")
  #d1c <- D1Client("DEV2", "urn:node:mnDevUCSB2")
  expect_false(is.null(d1cTest))
  #preferredNodes <- c("urn:node:mnDemo9")
  preferredNodes <- NA
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    dp <- new("DataPackage")
    # Create DataObject for the science data 
    sciObj <- new("DataObject", format="text/csv", mnNodeId=getMNodeId(d1cTest), filename=csvfile)
    # It's possible to set access rules for DataObject now, or for all DataObjects when they are uploaded to DataONE via uploadDataPackage
    expect_match(sciObj@sysmeta@identifier, "urn:uuid")
    sciObj <- setPublicAccess(sciObj)
    accessRules <- data.frame(subject=c("uid=smith,ou=Account,dc=example,dc=com", "uid=slaughter,o=unaffiliated,dc=example,dc=org"), permission=c("write", "changePermission"))
    sciObj <- addAccessRule(sciObj, accessRules)
    dp <- addMember(dp, sciObj)
    expect_true(is.element(sciObj@sysmeta@identifier, getIdentifiers(dp)))
    
    # Create metadata object that describes science data
    emlFile <- system.file("extdata/sample-eml.xml", package="dataone")
    metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", mnNodeId=getMNodeId(d1cTest), filename=emlFile)
    expect_match(metadataObj@sysmeta@identifier, "urn:uuid")
    dp <- addMember(dp, metadataObj)
    expect_true(is.element(metadataObj@sysmeta@identifier, getIdentifiers(dp)))
    
    # Associate the metadata object with the science object it describes
    dp <- insertRelationship(dp, subjectID=getIdentifier(metadataObj), objectIDs=getIdentifier(sciObj))
    
    # Upload the data package to DataONE    
    resourceMapId <- uploadDataPackage(d1cTest, dp, replicate=TRUE, numberReplicas=1, preferredNodes=preferredNodes,  public=TRUE, accessRules=accessRules)
    expect_true(!is.null(resourceMapId))

  } else {
      skip("This test requires valid authentication.")
  }
})

test_that("D1Client uploadDataPackage works for a minimal DataPackage", {
  
  # Test that a DataPackage with only one member (metadata in this case) and not
  # user defined relationships is created and uploaded correctly.
  skip_on_cran()
  library(dataone)
  library(datapack)
  # Create a csv file for the science object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  #d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
  expect_false(is.null(d1cTest))
  preferredNodes <- NA
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    dp <- new("DataPackage")
    
    # Create metadata object that describes science data
    emlFile <- system.file("extdata/sample-eml.xml", package="dataone")
    metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", mnNodeId=getMNodeId(d1cTest), filename=emlFile)
    expect_match(metadataObj@sysmeta@identifier, "urn:uuid")
    dp <- addMember(dp, metadataObj)
    expect_true(is.element(metadataObj@sysmeta@identifier, getIdentifiers(dp)))
    
    # Upload the data package to DataONE    
    resourceMapId <- uploadDataPackage(d1cTest, dp, replicate=TRUE, numberReplicas=1, preferredNodes=preferredNodes,  public=TRUE)
    expect_true(!is.null(resourceMapId))
  } else {
    skip("This test requires valid authentication.")
  }
})

test_that("D1Client updateDataPackage works", {
    
    # Test that a DataPackage with only one member (metadata in this case) and not
    # user defined relationships is created and uploaded correctly.
    skip_on_cran()
    library(dataone)
    library(datapack)
    library(xml2)
    library(digest)
    # Create a csv file for the science object
    #d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
    #d1c <- D1Client("STAGING2", "urn:node:mnTestKNB")
    #d1c <- D1Client("DEV2", "urn:node:mnDevUCSB1")
    expect_false(is.null(d1cTestKNB))
    preferredNodes <- NA
    # Set 'subject' to authentication subject, if available, so we will have permission to change this object
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTestKNB@mn))
    if (authValid) {
        if(dataone:::getAuthMethod(am, d1cTestKNB@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        dp <- new("DataPackage")
        
        # Create metadata object that describes science data
        emlFile <- system.file("extdata/strix-pacific-northwest.xml", package="dataone")
        metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", filename=emlFile)
        metadataId <- getIdentifier(metadataObj)
        # Associate the metadata object with each data object using the 'insertRelationships' method.
        # Since a relationship type (the predicate argument) is not specified, the default relationship
        # of 'cito:documents' is used, to indicate the the metadata object documents each data object.
        # See "http://vocab.ox.ac.uk/cito", for further information about the "Citation Type Ontology".
        dp <- addMember(dp, metadataObj)
        
        sourceData <- system.file("extdata/sample.csv", package="dataone")
        sourceObj <- new("DataObject", format="text/csv", filename=sourceData)
        dp <- addMember(dp, sourceObj, metadataObj)
        
        resolveURL <- sprintf("%s/%s/object", d1cTestKNB@mn@baseURL, d1cTestKNB@mn@APIversion)
        # Update the distribution URL in the metadata with the identifier that has been assigned to
        # this DataObject. This provides a direct link between the detailed information for this package
        # member and DataONE, which will assist DataONE in accessing and displaying this detailed information.
        xpathToURL <- "//dataTable/physical/distribution[../objectName/text()=\"OwlNightj.csv\"]/online/url"
        newURL <- sprintf("%s/%s", resolveURL, getIdentifier(sourceObj))
        dp <- updateMetadata(dp, metadataId, xpath=xpathToURL, newURL)
        metadataId <- selectMember(dp, name="sysmeta@formatId", value="eml://ecoinformatics.org/eml-2.1.1")
        metadataObj <- getMember(dp, metadataId)
        
        progFile <- system.file("extdata/filterSpecies.R", package="dataone")
        progObj <- new("DataObject", format="application/R", filename=progFile, mediaType="text/x-rsrc")
        dp <- addMember(dp, progObj, metadataObj)
        
        xpathToURL <- "//otherEntity/physical/distribution[../objectName/text()=\"filterObs.R\"]/online/url"
        newURL <- sprintf("%s/%s", resolveURL, getIdentifier(progObj))
        dp <- updateMetadata(dp, metadataId, xpath=xpathToURL, newURL)
        metadataId <- selectMember(dp, name="sysmeta@formatId", value="eml://ecoinformatics.org/eml-2.1.1")
        metadataObj <- getMember(dp, metadataId)
        
        outputData <- system.file("extdata/filteredSpecies.csv", package="dataone")
        outputObj <- new("DataObject", format="text/csv", filename=outputData)
        dp <- addMember(dp, outputObj, metadataObj)
        
        xpathToURL <- "//dataTable/physical/distribution[../objectName/text()=\"Strix-occidentalis-obs.csv\"]/online/url"
        newURL <- sprintf("%s/%s", resolveURL, getIdentifier(outputObj))
        dp <- updateMetadata(dp, metadataId, xpath=xpathToURL, newURL)
        
        # Upload the data package to DataONE
        newPkg <- uploadDataPackage(d1cTestKNB, dp, public=TRUE, quiet=TRUE, as="DataPackage")
        pkgId <- newPkg@resmapId
        expect_true(!is.na(pkgId))
        
        # Sleep for 90 secondsl to let indexing finish for the package. Because we are imposing a wait on this
        # package, this test is not suitable for use in CRAN. 
        
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("D1Client createD1Object works", {
  skip_on_cran()
  library(dataone)
  library(datapack)
  library(uuid)
  # Create a csv file for the science object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  #d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
  #d1c <- D1Client("SANDBOX2", "urn:node:mnDemo2")
  #d1c <- D1Client("DEV2", "urn:node:mnDevUCSB2")
  expect_false(is.null(d1cTestKNB))
  #preferredNodes <- c("urn:node:mnDemo9")
  preferredNodes <- NA
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTestKNB@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTestKNB@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    sdf <- read.csv(csvfile)
    # Create DataObject for the science data 
    suppressWarnings(stf <- charToRaw(convert.csv(d1cTestKNB, sdf)))
    sciId <- sprintf("urn:uuid:%s", UUIDgenerate())
    # Create D1Object for the science data 
    # Suppress .Deprecated warnings
    id <- sprintf("urn:uuid:%s", UUIDgenerate())
    suppressWarnings(stf <- charToRaw(convert.csv(d1cTestKNB, testdf)))
    suppressWarnings(sciObj <- new("D1Object", id=sciId, format="text/csv", data=stf, mnNodeId=getMNodeId(d1cTestKNB)))
    # It's possible to set access rules for DataObject now, or for all DataObjects when they are uploaded to DataONE via uploadDataPackage
    expect_match(sciObj@dataObject@sysmeta@identifier, "urn:uuid")
    suppressWarnings(sciObj <- setPublicAccess(sciObj))
    # Upload the data object to DataONE
    suppressWarnings(success <- createD1Object(d1cTestKNB, sciObj))
    expect_true(success)
    # Now see if we can download the object from DataONE
  } else {
    skip("This test requires valid authentication.")
  }
})

test_that("D1Client getD1Object works", {
  library(dataone)
  library(digest)
  
  am <- AuthenticationManager()
  #d1c <- D1Client("PROD", "urn:node:KNB")
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cKNB@cn))
  # Skip if Mac OS X and certificate.
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cKNB@cn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  expect_false(is.null(d1cKNB))
  expect_match(class(d1cKNB), "D1Client")
  expect_match(d1cKNB@cn@baseURL, "https://cn.dataone.org/cn")
    
  # Try retrieving a known object from the PROD environment
  pid <- "solson.5.1"
  suppressWarnings(dataObj <- getD1Object(d1cKNB, pid))
  expect_match(class(dataObj)[1], "DataObject")
  expect_match(class(dataObj@sysmeta), "SystemMetadata")
  expect_match(getIdentifier(dataObj), pid)
  expect_match(getFormatId(dataObj), "text/csv")
  data <- getData(dataObj)
  sha1 <- digest(data, algo="md5", serialize=FALSE, file=FALSE)
  expect_match(sha1, dataObj@sysmeta@checksum)
})


test_that("D1Client d1SolrQuery works", {
  library(dataone)
  #d1c <- D1Client("PROD")
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cProd@cn))
  # Skip if Mac OS X and certificate.
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cProd@cn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  queryParams <- list(q="id:doi*", fq="abstract:hydrocarbon", rows="2", wt="xml")
  suppressWarnings(result <- d1SolrQuery(d1cProd, queryParams))
  expect_match(class(result)[1], "XMLInternalDocument")
  resList <- xmlToList(result)
  expect_true(length(resList) > 0)
})

test_that("D1Client listMemberNodes() works", {
  library(dataone)
  #d1c <- D1Client("PROD")
  nodelist <- listMemberNodes(d1cProd)
  expect_that(length(nodelist) > 0, is_true())
  expect_match(class(nodelist[[1]]), "Node")
  expect_match(nodelist[[1]]@identifier, "urn:node:")
  expect_match(nodelist[[1]]@type, "cn|mn")
  expect_match(nodelist[[1]]@state, "up")
  expect_match(nodelist[[length(nodelist)]]@identifier, "urn:node:")
  expect_match(nodelist[[length(nodelist)]]@baseURL, "http")
  expect_match(nodelist[[length(nodelist)]]@subject, "urn:node:")
  expect_match(nodelist[[length(nodelist)]]@type, "cn|mn")
})

test_that("D1Client d1IdentifierSearch works", {
  library(dataone)
  am <- AuthenticationManager()
  #d1c <- D1Client("PROD")
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cProd@cn))
  # Skip if Mac OS X and certificate.
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cProd@cn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  queryParams <- "q=id:doi*&fq=abstract:kelp&rows=2" 
  suppressWarnings(result <- d1IdentifierSearch(d1cProd, queryParams))
  expect_match(class(result), "character")
  expect_equal(length(result), 2)
})

test_that("D1Client createDataPackage works", {
  skip_on_cran()
  library(dataone)
  library(datapack)
  library(uuid)
  
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  #d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
  #d1c <- D1Client("SANDBOX2", "urn:node:mnDemo2")
  #d1c <- D1Client("DEV2", "urn:node:mnDevUCSB2")
  expect_false(is.null(d1cTest))
  #preferredNodes <- c("urn:node:mnDemo9")
  preferredNodes <- NA
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    dp <- new("DataPackage")
    # Create metadata object that describes science data
    emlFile <- system.file("extdata/sample-eml.xml", package="dataone")
    emlChar <- readLines(emlFile)
    emlRaw <- charToRaw(paste(emlChar, collapse="\n"))
    emlId <- sprintf("urn:uuid:%s", UUIDgenerate())
    suppressWarnings(metadataObj <- new("D1Object", id=emlId, format="eml://ecoinformatics.org/eml-2.1.1", data=emlRaw, mnNodeId=getMNodeId(d1cTest)))
    expect_match(metadataObj@dataObject@sysmeta@identifier, "urn:uuid")
    suppressWarnings(dp <- addData(dp, metadataObj))
    expect_true(is.element(metadataObj@dataObject@sysmeta@identifier, getIdentifiers(dp)))
    
    sdf <- read.csv(csvfile)
    # Create DataObject for the science data 
    suppressWarnings(stf <- charToRaw(convert.csv(d1cTest, sdf)))
    sciId <- sprintf("urn:uuid:%s", UUIDgenerate())
    suppressWarnings(sciObj <- new("D1Object", id=sciId, format="text/csv", data=stf, mnNodeId=getMNodeId(d1cTest)))
    # It's possible to set access rules for DataObject now, or for all DataObjects when they are uploaded to DataONE via uploadDataPackage
    expect_match(sciObj@dataObject@sysmeta@identifier, "urn:uuid")
    suppressWarnings(dp <- addData(dp, sciObj, metadataObj))
    expect_true(is.element(sciObj@dataObject@sysmeta@identifier, getIdentifiers(dp)))
    
    # Upload the data package to DataONE    
    suppressWarnings(resourceMapId <- createDataPackage(d1cTest, dp, replicate=TRUE, public=TRUE))
    expect_true(!is.null(resourceMapId))
    
  } else {
    skip("This test requires valid authentication.")
  }
})

test_that("D1Client updateDataPackage works for a metadata only DataPackage", {
    
    skip_on_cran()
    # Test that a DataPackage with only one member (metadata in this case) and not
    # user defined relationships is created and uploaded correctly.
    # This is a long running test, so it should be run manually, which means
    # running "test_file("tests/testthat/packageUpdate.R"), as this file is not
    # run by default by testthat due to the name not including 'test*'
    library(dataone)
    library(datapack)
    library(xml2)
    library(digest)
    # Create a csv file for the science object
    #d1c <- D1Client("STAGING", "urn:node:mnStageUCSB2")
    #d1c <- D1Client("DEV2", "urn:node:mnDevUCSB1")
    #d1c <- D1Client("STAGING2", "urn:node:mnTestKNB")
    #d1c <- D1Client("SANDBOX", "urn:node:mnSandboxUCSB1")
    #d1c <- D1Client("DEV", "urn:node:mnDemo6")
    #d1c <- D1Client("DEV2", "urn:node:mnDevUCSB1")
    expect_false(is.null(d1cTest))
    preferredNodes <- NA
    # Set 'subject' to authentication subject, if available, so we will have permission to change this object
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
    if (authValid) {
        if(dataone:::getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        dp <- new("DataPackage")
        
        # Create metadata object that describes science data
        emlFile <- system.file("extdata/strix-pacific-northwest.xml", package="dataone")
        metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", filename=emlFile)
        metadataId <- getIdentifier(metadataObj)
        # Associate the metadata object with each data object using the 'insertRelationships' method.
        # Since a relationship type (the predicate argument) is not specified, the default relationship
        # of 'cito:documents' is used, to indicate the the metadata object documents each data object.
        # See "http://vocab.ox.ac.uk/cito", for further information about the "Citation Type Ontology".
        dp <- addMember(dp, metadataObj)
        
        pkgId <- uploadDataPackage(d1cTest, dp, public=TRUE, quiet=TRUE)
        expect_true(!is.na(pkgId))
        
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("D1Client downloadObject", {
  library(dataone)
  #cli <- D1Client("PROD", "urn:node:KNB")
  expect_false(is.null(d1cKNB))
  expect_match(class(d1cKNB), "D1Client")
  expect_match(d1cKNB@cn@baseURL, "https://cn.dataone.org/cn")
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cKNB@mn))
  if(authValid) {
    # Skip if Mac OS and X.509 Certificate
    if(dataone:::getAuthMethod(am, d1cKNB@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  
  # Try downloading a known object from the PROD environment
  pid <- "solson.5.1"
  path <- tempdir()
  file <- downloadObject(d1cKNB, pid, path)
  expect_match(class(file), "path")
  expect_true(file.exists(file))
  unlink(file)
})


