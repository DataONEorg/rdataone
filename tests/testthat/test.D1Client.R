test_that("dataone library loads", {
	expect_true(require(dataone))
})
test_that("D1Client constructors", {
  skip_on_cran()
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
        cli <- new("D1Client", cn=cnStaging, mn=getMNode(cnStaging, "urn:node:mnTestKNB"))
        expect_false(is.null(cli))
        expect_match(class(cli), "D1Client")
        expect_match(cli@cn@baseURL, "https://cn-stage.test.dataone.org/cn")
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
  skip_on_cran()
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
  cli <- D1Client("STAGING", "urn:node:mnTestKNB")
  expect_match(getMNodeId(cli), "urn:node:mnTestKNB")
  # Test setMNodeId
  cli <- new("D1Client", env="STAGING")
  cli <- setMNodeId(cli, "urn:node:mnTestKNB")
  expect_match(cli@mn@identifier, "urn:node:mnTestKNB")
})

test_that("D1Client getDataObject", {
    skip_on_cran()
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
    obj <- getDataObject(d1cKNB, pid, checksumAlgorithm="SHA-256")
    cname <- class(obj)[1]
    expect_match(cname, "DataObject")
    expect_match(class(obj@sysmeta), "SystemMetadata")
    expect_match(getIdentifier(obj), pid)
    expect_match(getFormatId(obj), "text/csv")
    data <- getData(obj)
    sha256 <- digest(data, algo="sha256", serialize=FALSE, file=FALSE)
    expect_match(sha256, obj@sysmeta@checksum)
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
        # See "http://purl.org/spar/cito", for further information about the "Citation Type Ontology".
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

test_that("D1Client updateDataPackage with new package using previously uploaded objects works", {
  
  # Test the typical workflow of creating a DataONE package by first uploading all data objects for the package,
  # then creating a package from the already uploaded objects. 
  skip_on_cran()
  library(dataone)
  library(datapack)
  library(xml2)
  library(digest)
  expect_false(is.null(d1cTestKNB))
  preferredNodes <- NA
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTestKNB@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTestKNB@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    
    # First upload objects to DataONE that will be collected into a package
    sourceData <- system.file("extdata/OwlNightj.csv", package="dataone")
    sourceObj <- new("DataObject", format="text/csv", filename=sourceData)
    sourceObj <- addAccessRule(sourceObj, "http://orcid.org/0000-0002-2192-403X", "changePermission")
    sourceId <- uploadDataObject(d1cTestKNB, sourceObj, public=T, quiet=T)
    expect_true(!is.na(sourceId))
    
    progFile <- system.file("extdata/filterObs.R", package="dataone")
    progObj <- new("DataObject", format="application/R", filename=progFile, mediaType="text/x-rsrc")
    progObj <- addAccessRule(progObj, "http://orcid.org/0000-0002-2192-403X", "changePermission")
    progId <- uploadDataObject(d1cTestKNB, progObj, public=T, quiet=T)
    expect_true(!is.na(progId))
    
    outputData <- system.file("extdata/Strix-occidentalis-obs.csv", package="dataone")
    outputObj <- new("DataObject", format="text/csv", filename=outputData)
    outputObj <- addAccessRule(outputObj, "http://orcid.org/0000-0002-2192-403X", "changePermission")
    outputId <- uploadDataObject(d1cTestKNB, outputObj, public=T, quiet=T)
    expect_true(!is.na(outputId))
    
    # Create a new package, and download each member (lazyLoaded) that was just uploaded, then add them
    # to the package and upload. This workflow does not require that package members are downloaded with
    # lazyLoad, this is done here just for efficiency. If a package member is downloaded without lazyLoad,
    # it will not be-reuploaded when the package is uploaded, unless it has been updated (i.e. updated contents,
    # or sysmeta).
    pkg <- new("DataPackage")
    # Create metadata object that describes the package
    emlFile <- system.file("extdata/strix-pacific-northwest.xml", package="dataone")
    metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", filename=emlFile)
    metadataObj <- addAccessRule(metadataObj, "http://orcid.org/0000-0002-2192-403X", "changePermission")
    pkg <- addMember(pkg, metadataObj)
    metadataId <- getIdentifier(metadataObj)
    
    newSourceObj <- getDataObject(d1cTestKNB, sourceId, lazyLoad=T, quiet=T)
    pkg  <- addMember(pkg, newSourceObj, metadataObj)
    
    newProgObj <- getDataObject(d1cTestKNB, progId, lazyLoad=T, quiet=T)
    pkg <- addMember(pkg, newProgObj, metadataObj)
    
    newOutputObj <- getDataObject(d1cTestKNB, outputId, lazyLoad=T, quiet=T)
    pkg <- addMember(pkg, newOutputObj, metadataObj)
    
    resourceMapId <- uploadDataPackage(d1cTestKNB, pkg, public=TRUE, quiet=T)
    expect_false(is.na(resourceMapId))
    
    # Now test that we can download the newly created package and add an existing object
    # Now add a new package member that was omitted from the original package
    auxFile <- system.file("extdata/WeatherInf.txt", package="dataone")
    auxObj <- new("DataObject", format="text/plain", file=auxFile)
    auxObj <- addAccessRule(auxObj, "http://orcid.org/0000-0002-2192-403X", "changePermission")
    auxId <- uploadDataObject(d1cTestKNB, auxObj, public=T, quiet=T)
    expect_true(!is.na(auxId))
    
    # Have to sleep just a bit, as indexing can take awhile to complete
    # Keep trying for ten seconds for the package to be indexed
    done <- FALSE
    trys <- 0
    while(!done) {
      if(trys > 10) break
      Sys.sleep(1)
      queryParams <- sprintf('q=id:"%s"', resourceMapId)
      result <- query(d1cTestKNB@mn, queryParams, as="list")
      # Now download the package that was just created, and ensure that the checksums are all the
      # requested type.
      if(length(result) == 0) {
        trys <- trys + 1
        next
      } else {
        done <- TRUE
      }
    
      newAuxObj <- getDataObject(d1cTestKNB, auxId, lazyLoad=T, quiet=T)
      editPkg <- getDataPackage(d1cTestKNB, identifier=resourceMapId, lazyLoad=TRUE, quiet=TRUE)
    }
    
    expect_true(done)
    
    editPkg <- addMember(editPkg, newAuxObj, metadataObj)
    newResmapId <- uploadDataPackage(d1cTestKNB, editPkg, public=TRUE, quiet=T)
    expect_false(is.na(newResmapId))
    expect_false(resourceMapId == newResmapId)
  } else {
    skip("This test requires valid authentication.")
  }
})

test_that("D1Client getDataPackage with checksumAlgorithm specified works", {
  
  # Test that a DataPackage with only one member (metadata in this case) and not
  # user defined relationships is created and uploaded correctly.
  skip_on_cran()
  library(dataone)
  library(datapack)
  library(xml2)
  library(digest)
  # Create a csv file for the science object
  expect_false(is.null(d1cTestKNB))
  preferredNodes <- NA
  # Set 'subject' to authentication subject, if available, so we will have permission to change this object
  am <- AuthenticationManager()
  suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTestKNB@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTestKNB@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    sha256 <- "SHA-256"
    md5 <- "MD5"
    checksumAlgorithm <- sha256
    dp <- new("DataPackage")
    
    # Create metadata object that describes science data
    emlFile <- system.file("extdata/strix-pacific-northwest.xml", package="dataone")
    metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", filename=emlFile, checksum=checksumAlgorithm)
    metadataId <- getIdentifier(metadataObj)
    
    dp <- addMember(dp, metadataObj)
    
    sourceData <- system.file("extdata/sample.csv", package="dataone")
    sourceObj <- new("DataObject", format="text/csv", filename=sourceData, checksum=checksumAlgorithm)
    dp <- addMember(dp, sourceObj, metadataObj)
    
    progFile <- system.file("extdata/filterSpecies.R", package="dataone")
    progObj <- new("DataObject", format="application/R", filename=progFile, mediaType="text/x-rsrc", checksum=checksumAlgorithm)
    dp <- addMember(dp, progObj, metadataObj)
    
    outputData <- system.file("extdata/filteredSpecies.csv", package="dataone")
    outputObj <- new("DataObject", format="text/csv", filename=outputData, checksum=checksumAlgorithm)
    dp <- addMember(dp, outputObj, metadataObj)
    
    # Upload the data package to DataONE
    pkgId <- uploadDataPackage(d1cTestKNB, dp, public=TRUE, quiet=TRUE)
    expect_true(!is.na(pkgId))
    
    # Have to sleep just a bit, as indexing can take awhile to complete
    # Keep trying for ten seconds for the package to be indexed
    done <- FALSE
    trys <- 0
    while(!done) {
      if(trys > 10) break
      Sys.sleep(1)
      queryParams <- sprintf('q=id:"%s"', pkgId)
      result <- query(d1cTestKNB@mn, queryParams, as="list")
      # Now download the package that was just created, and ensure that the checksums are all the
      # requested type.
      if(length(result) == 0) {
        trys <- trys + 1
        next
      } else {
        done <- TRUE
      }
      pkg <- getDataPackage(d1cTestKNB, identifier=pkgId, lazyLoad=TRUE, limit="0MB", quiet=TRUE, checksumAlgorithm=sha256)
      algorithms <- getValue(pkg, name="sysmeta@checksumAlgorithm")
      expect_true(all(algorithms == sha256))
    
      # Download the package again, requesting a different checksum type, and ensure that the checksums are all the
      # new type.
      pkg <- getDataPackage(d1cTestKNB, identifier=pkgId, lazyLoad=TRUE, limit="0MB", quiet=TRUE, checksumAlgorithm=md5)
      algorithms <- getValue(pkg, name="sysmeta@checksumAlgorithm")
      expect_true(all(algorithms==md5))
    }
    
    expect_true(done)
    
  } else {
    skip("This test requires valid authentication.")
  }
})

test_that("D1Client listMemberNodes() works", {
  skip_on_cran()
  library(dataone)
  #d1c <- D1Client("PROD")
  nodelist <- listMemberNodes(d1cProd)
  expect_true(length(nodelist) > 0)
  expect_match(class(nodelist[[1]]), "Node")
  expect_match(nodelist[[1]]@identifier, "urn:node:")
  expect_match(nodelist[[1]]@type, "cn|mn")
  expect_match(nodelist[[1]]@state, "up")
  expect_match(nodelist[[length(nodelist)]]@identifier, "urn:node:")
  expect_match(nodelist[[length(nodelist)]]@baseURL, "http")
  expect_match(nodelist[[length(nodelist)]]@subject, "urn:node:")
  expect_match(nodelist[[length(nodelist)]]@type, "cn|mn")
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
        # See "http://purl.org/spar/cito", for further information about the "Citation Type Ontology".
        dp <- addMember(dp, metadataObj)
        
        pkgId <- uploadDataPackage(d1cTest, dp, public=TRUE, quiet=TRUE)
        expect_true(!is.na(pkgId))
        
        done <- FALSE
        trys <- 0
        while(!done) {
          if(trys > 10) break
          Sys.sleep(1)
          queryParams <- sprintf('q=id:"%s"', pkgId)
          result <- query(d1cTest@mn, queryParams, as="list")
          # Now download the package that was just created, and ensure that the checksums are all the
          # requested type.
          if(length(result) == 0) {
            trys <- trys + 1
            next
          } else {
            done <- TRUE
          }
        
          # Test the download by specifying the metadata id of the package. The 'getDataPackage()' function
          # should be able determine the package id based on the metadata id.
          testPkg <- getDataPackage(d1cTest, metadataId, quiet=T)
          expect_equal(pkgId, testPkg@resmapId)
        }
        
        expect_true(done)
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("D1Client downloadObject", {
  skip_on_cran()
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

test_that("D1Client uploadDataPackage public argument works", {
    
    # Test that a DataPackage with only one member (metadata in this case) and not
    # user defined relationships is created and uploaded correctly.
    skip_on_cran()
    library(dataone)
    library(datapack)
    # Create a csv file for the science object
    testdf <- data.frame(x=1:10,y=11:20)
    csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
    write.csv(testdf, csvfile, row.names=FALSE)

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
        
        # give metadata object an access policy without public read
        metadataObj <- addAccessRule(metadataObj, "CN=arctic-data-admins,DC=dataone,DC=org", "read")
        metadataObj <- addAccessRule(metadataObj, "CN=arctic-data-admins,DC=dataone,DC=org", "write")
        metadataObj <- addAccessRule(metadataObj, "CN=arctic-data-admins,DC=dataone,DC=org", "changePermission")
        dp <- addMember(dp, metadataObj)
        
        
        expect_true(is.element(metadataObj@sysmeta@identifier, getIdentifiers(dp)))
        
        # Upload the data package to DataONE with public set to TRUE
        resourceMapId <- uploadDataPackage(d1cTest, dp, replicate=TRUE, numberReplicas=1, preferredNodes=preferredNodes,  public=TRUE)
        expect_true(!is.null(resourceMapId))
        
        # check that all members of the package have public read
        sys_rm <- getSystemMetadata(d1cTest@mn, resourceMapId)
        sys_mo <- getSystemMetadata(d1cTest@mn, metadataObj@sysmeta@identifier)
        
        expect_true("public" %in% sys_rm@accessPolicy$subject)
        expect_true("public" %in% sys_mo@accessPolicy$subject)
        
    } else {
        skip("This test requires valid authentication.")
    }
})


test_that("D1Client uploadDataPackage doesn't change the rightsHolder", {
    
    # Test that a DataPackage with only one member (metadata in this case) and not
    # user defined relationships is created and uploaded correctly.
    skip_on_cran()
    library(dataone)
    library(datapack)
    # Create a csv file for the science object

    # Set 'subject' to authentication subject, if available, so we will have permission to change this object
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
    if (authValid) {
        if(dataone:::getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        dp <- new("DataPackage")
        
        # Create metadata object that describes science data
        emlFile <- system.file("extdata/sample-eml.xml", package="dataone")
        metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", mnNodeId=getMNodeId(d1cTest), filename=emlFile)
        
        # set rightsHolder on metadata to a test ORCID
        metadataObj@sysmeta@rightsHolder <- "http://orcid.org/0000-0000-0000-0000"

        dp <- addMember(dp, metadataObj)
        
        # set rightsHolder on resource map to a test ORCID
        dp@sysmeta@rightsHolder <- "http://orcid.org/0000-0000-0000-0000"
        
        # Upload the data package to DataONE with public set to TRUE
        resourceMapId <- uploadDataPackage(d1cTest, dp, replicate=TRUE, numberReplicas=1, public=TRUE)
        
        # check that all members of the package have public read
        sys_rm <- getSystemMetadata(d1cTest@mn, resourceMapId)
        sys_mo <- getSystemMetadata(d1cTest@mn, metadataObj@sysmeta@identifier)
        
        expect_equal("http://orcid.org/0000-0000-0000-0000", sys_rm@rightsHolder)
        expect_equal("http://orcid.org/0000-0000-0000-0000", sys_rm@rightsHolder)
        
    } else {
        skip("This test requires valid authentication.")
    }
})



