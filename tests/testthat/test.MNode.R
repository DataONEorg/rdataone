test_that("dataone library loads", {
	expect_true(require(dataone))
})
test_that("MNode constructors", {
  skip_on_cran()
	library(dataone)
	mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
	mn <- MNode(mn_uri)
	expect_match(mn@endpoint, mn_uri)
})
test_that("MNode getCapabilities()", {
    skip_on_cran()
    library(dataone)
    library(XML)
    #mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
    #mn <- MNode(mn_uri)
    xml <- getCapabilities(mnKNB)
    val <- xmlName(xmlRoot(xml))
    expect_match(val, "node")
    expect_match(mnKNB@identifier, "urn:node")
})
test_that("MNode getObject(), getChecksum()", {
    skip_on_cran()
    library(dataone)
    #mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
    #mn <- MNode(mn_uri)
    pid <- "doi:10.5063/F1QN64NZ"
    bytes <- getObject(mnKNB, pid)
    xml <- xmlParseDoc(rawToChar(bytes), asText=TRUE)
    cname <- class(xml)[1]
    expect_match(cname, "XML")
    pid <- "solson.5.1"
    obj <- getObject(mnKNB, pid)
    df <- read.csv(text=rawToChar(obj))
    cname <- class(df)[1]
    expect_match(cname, "data.frame")
    # 'cn' is defined in 'helper-base.R' for all tests
    knb <- getMNode(cnProd, "urn:node:KNB")
    pid <- "doi:10.5063/F1QN64NZ"
    bytes <- getObject(mnKNB, pid)
    xml <- xmlParseDoc(rawToChar(bytes), asText=TRUE)
    cname <- class(xml)[1]
    expect_match(cname, "XML")
    chksum <- getChecksum(mnKNB, pid)
    expect_type(chksum, "character")
    expect_false(is.null(chksum))
})
test_that("MNode getSystemMetadata()", {
    skip_on_cran()
    library(dataone)
    #mn <- getMNode(cnProd, "urn:node:KNB")
    pid <- "doi:10.5063/F1QN64NZ"
    sysmeta <- getSystemMetadata(mnKNB, pid)
    cname <- class(sysmeta)[1]
    expect_match(cname, "SystemMetadata")
    expect_match(sysmeta@identifier, "doi:10.5063/F1QN64NZ")
})

test_that("MNode generateIdentifier()", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    #cn <- CNode("STAGING")
    #mn <- getMNode(cn, "urn:node:mnStageUCSB2")  
    # Suppress openssl, cert missing warnings
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, mnTest))
    if(authValid) {
        if(dataone:::getAuthMethod(am, mnTest) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        newid <- generateIdentifier(mnTest, "UUID")
        cname <- class(newid)
        expect_match(cname, "character")
        expect_match(newid, "urn:uuid:")
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("MNode generateIdentifier() on API v1 node", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    # Currently this is a v1 node, so only X.509 certs work for authentication
    # so this test should find and use a cert if it is available.
    #cn <- CNode("STAGING")
    #suppressWarnings(mn <- getMNode(cn, "urn:node:mnStageUCSB2"))
    #if(is.null(mn)) skip("Member node urn:node:mnStageUCSB2 not available")
    # Suppress openssl, cert missing warnings
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, mnTest))
    if(authValid) {
        if(dataone:::getAuthMethod(am, mnTest) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        newid <- generateIdentifier(mnTest, "UUID")
        cname <- class(newid)
        expect_match(cname, "character")
        expect_match(newid, "urn:uuid:")
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("MNode describeObject()", {
    skip_on_cran()
    library(dataone)
    #mn_uri <- "https://knb.ecoinformatics.org/knb/d1/mn/v2"
    #mn <- MNode(mn_uri)
    res <- describeObject(mnKNB, "knb.473.1")
    expect_type(res, "list")
    expect_equal(res$`content-type`, "text/xml")
})

test_that("MNode describeObject() with authentication", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    library(uuid)
    library(digest)
    # Suppress openssl, cert missing warnings
    am <- AuthenticationManager()
    suppressMessages(authValid <- dataone:::isAuthValid(am, mnTest))
    if(authValid) {
        if(dataone:::getAuthMethod(am, mnTest) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        user <- dataone:::getAuthSubject(am, mnTest)
        newid <- generateIdentifier(mnTest, "UUID")
        cname <- class(newid)
        testdf <- data.frame(x=1:10,y=11:20)
        csvfile <- paste(tempfile(), ".csv", sep="")
        write.csv(testdf, csvfile, row.names=FALSE)
        # Create SystemMetadata for the object
        format <- "text/csv"
        size <- file.info(csvfile)$size
        sha256 <- digest(csvfile, algo="sha256", serialize=FALSE, file=TRUE)
        # specify series id for this sysmeta. This will only be used if uploading to a DataONE v2 node
        sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, checksum=sha256,
                       originMemberNode=mnTest@identifier, authoritativeMemberNode=mnTest@identifier)
        # sysmeta <- addAccessRule(sysmeta, "public", "read")
        # Upload the data to the MN using createObject(), checking for success and a returned identifier
        # The object is not created with public read access so that we can test that an authenticated
        # describeObject, i.e. can't read the object unless you have read access, in this case via
        # being the rightsholder.
        createdId <- createObject(mnTest, newid, csvfile, sysmeta)
        expect_false(is.null(createdId))
        res <- describeObject(mnTest, newid)
        expect_type(res, "list")
        expect_equal(res$`content-type`, "text/csv")
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("MNode createObject(), updateObject(), archive()", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    library(digest)
    library(datapack)
    library(uuid)
    library(XML)
    #cn <- CNode("SANDBOX")
    #cn <- CNode("DEV2")
    #cn <- CNode("STAGING")
    # mnDemo1 is api v1 on 20151208, but that could change
    # Use this v1 node to test with both a current token available
    # and a certificate.
    #mnId <- "urn:node:mnSandboxUCSB2"
    #mnId <- "urn:node:mnDevUCSB2"
    #mnId <- "urn:node:mnStageUCSB2"
    #mn <- getMNode(cn, mnId)
    am <- AuthenticationManager()
    # Suppress openssl, cert missing warnings
    suppressMessages(authValid <- dataone:::isAuthValid(am, mnTest))
    if (authValid) {
        if(dataone:::getAuthMethod(am, mnTest) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        user <- dataone:::getAuthSubject(am, mnTest)
        newid <- generateIdentifier(mnTest, "UUID")
        cname <- class(newid)
        expect_match(cname, "character")
        expect_match(newid, "urn:uuid:")
        expect_match(user, "cilogon|dataone|orcid")
        # Create a data object, and convert it to csv format
        testdf <- data.frame(x=1:10,y=11:20)
        csvfile <- paste(tempfile(), ".csv", sep="")
        write.csv(testdf, csvfile, row.names=FALSE)
        
        # Create SystemMetadata for the object
        format <- "text/csv"
        size <- file.info(csvfile)$size
        sha256 <- digest(csvfile, algo="sha256", serialize=FALSE, file=TRUE)
        # specify series id for this sysmeta. This will only be used if uploading to a DataONE v2 node
        seriesId <- UUIDgenerate()
        sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, checksum=sha256,
                       originMemberNode=mnTest@identifier, authoritativeMemberNode=mnTest@identifier, seriesId=seriesId)
        sysmeta <- addAccessRule(sysmeta, "public", "read")
        expect_equal(sysmeta@checksum, sha256)
        expect_equal(sysmeta@originMemberNode, mnTest@identifier)
        expect_equal(sysmeta@authoritativeMemberNode, mnTest@identifier)
        
        # Upload the data to the MN using createObject(), checking for success and a returned identifier
        createdId <- createObject(mnTest, newid, csvfile, sysmeta)
        expect_false(is.null(createdId))
        expect_match(createdId, newid)
        
        # Update the object with a new version
        updateid <- generateIdentifier(mnTest, "UUID")
        testdf <- data.frame(x=1:20,y=11:30)
        csvfile <- paste(tempfile(), ".csv", sep="")
        write.csv(testdf, csvfile, row.names=FALSE)
        size <- file.info(csvfile)$size
        sha256 <- digest(csvfile, algo="sha256", serialize=FALSE, file=TRUE)
        sysmeta@identifier <- updateid
        sysmeta@size <- size
        sysmeta@checksum <- sha256
        sysmeta@obsoletes <- newid
        newId <- updateObject(mnTest, newid, csvfile, updateid, sysmeta)
        expect_false(is.null(newId))
        expect_match(newId, updateid)
        updsysmeta <- getSystemMetadata(mnTest, updateid)
        expect_match(class(updsysmeta)[1], "SystemMetadata")
        expect_match(updsysmeta@obsoletes, newid)
        
        # Now get the sysmeta using the seriesId, if supported
        if(mnTest@APIversion >= "v2") {
            headSysmeta <- getSystemMetadata(mnTest, seriesId)
            expect_match(class(headSysmeta)[1], "SystemMetadata")
            expect_match(updsysmeta@identifier, headSysmeta@identifier)
        }
        
        # Archive the object
        response <- archive(mnTest, newid)
        expect_match(response, newid)
        newsysmeta <- getSystemMetadata(mnTest, newid)
        expect_match(class(newsysmeta)[1], "SystemMetadata")
        expect_true(newsysmeta@archived)
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("MNode archive() return error response messages", {
    skip_on_cran()
    am <- AuthenticationManager()
    suppressMessages({authValid <- dataone:::isAuthValid(am, mnTest)})
    
    if (authValid) {
        pid <- uuid::UUIDgenerate()
        expect_warning({
            archive(mnTest, pid)
        }, paste0("No system metadata could be found for given PID: ", pid))
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("MNode createObject() with in-memory object", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    library(digest)
    library(datapack)
    library(uuid)
    library(XML)
    #cn <- CNode("SANDBOX")
    #cn <- CNode("DEV2")
    #cn <- CNode("STAGING")
    #mnId <- "urn:node:mnStageUCSB2"
    #mn <- getMNode(cn, mnId)
    am <- AuthenticationManager()
    # Suppress openssl, cert missing warnings
    suppressMessages(authValid <- dataone:::isAuthValid(am, mnTest))
    if (authValid) {
        if(dataone:::getAuthMethod(am, mnTest) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        user <- dataone:::getAuthSubject(am, mnTest)
        newid <- sprintf("urn:uuid:%s", UUIDgenerate())
        
        # Create an in-memory object (vs file object)
        testdf <- data.frame(x=1:10,y=11:20)
        con <- textConnection(NULL, "w")
        write.csv(testdf, file=con, row.names = FALSE)
        csvbuf <- textConnectionValue(con)
        close(con)
        # Data must be type "raw" to be uploadable by createObject()
        csvdata <- charToRaw(paste(csvbuf, collapse="\n"))
        
        # Create SystemMetadata for the object
        format <- "text/csv"
        size <- length(csvdata)
        sha256 <- digest(csvdata, algo="sha256", serialize=FALSE, file=FALSE)
        # specify series id for this sysmeta. This will only be used if uploading to a DataONE v2 node
        
        sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, checksum=sha256)
        sysmeta <- addAccessRule(sysmeta, "public", "read")
        expect_equal(sysmeta@checksum, sha256)
        
        # Upload the data to the MN using createObject(), checking for success and a returned identifier
        createdId <- createObject(mnTest, newid, sysmeta = sysmeta, dataobj=csvdata)
        expect_false(is.null(createdId))
        expect_match(createdId, newid)
        newSysmeta <- getSystemMetadata(mnTest, pid=newid)
        expect_match(sysmeta@formatId, newSysmeta@formatId)
        expect_equal(sysmeta@size, newSysmeta@size)
    } else {
        skip("This test requires valid authentication.")
    }
})

test_that("MNode createObject() works for large files", {
    # This line has to be commented out to run this test.
    skip("Skip large file testing.")
    if (grepl("Darwin", Sys.info()['sysname'])) {
        skip("fallocate not available on Mac")
    }
    library(dataone)
    library(digest)
    library(httr)
    newid <- generateIdentifier(mnTest, "UUID")
    cname <- class(newid)
    expect_match(cname, "character")
    expect_match(newid, "urn:uuid:")
    # Ensure the user is logged in before running the tests
    # Set 'user' to authentication subject, if available, so we will have permission to change this object
    am <- AuthenticationManager()
    # Suppress openssl, cert missing warnings
    suppressMessages(authValid <- dataone:::isAuthValid(am, mnTest))
    if (authValid) {
      #if(getAuthMethod(am, mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
      user <- dataone:::getAuthSubject(am, mnTest)
      # TODO: Create a large data object using fallocate through a system call (only on linux)
      csvfile <- 'testdata.csv'
      csvsize <- '4G'
      system(paste("fallocate -l", csvsize, csvfile))
      
      # On a mac, use truncate instead (slower, and produces a sparse file)
      #csvsize <- '4294967296'    # truncate needs the size arg in bytes
      #system(paste("truncate", csvfile, csvsize))
      
      # Create SystemMetadata for the object
      format <- "text/csv"
      size <- file.info(csvfile)$size
      sha256 <- digest(csvfile, algo="sha256", serialize=FALSE, file=TRUE)
      sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, checksum=sha256, originMemberNode=mnTest@identifier, authoritativeMemberNode=mnTest@identifier)
      sysmeta <- addAccessRule(sysmeta, "public", "read")
      expect_equal(sysmeta@checksum, sha256)
      expect_equal(sysmeta@originMemberNode, mnTest@identifier)
      expect_equal(sysmeta@authoritativeMemberNode, mnTest@identifier)
      
      # Upload the data to the MN using createObject(), checking for success and a returned identifier
      # Note: createObject() will ensure that sysmeta@submitter, sysmeta@rightsHolder are set
      createdId <- createObject(mnTest, newid, csvfile, sysmeta)
      expect_false(is.null(createdId))
      expect_match(createdId, newid) 
      
      # Remove the big data file we createObjectd locally
      unlink(csvfile)
    } else {
      skip("This test requires valid authentication.")
    }
})

test_that("MNode getPackage() works", {
  skip_on_cran()
  library(uuid)
  # This test can exceed the CRAN test running time limits
  resMapPid <- "resourceMap_lrw.3.5"
  am <- AuthenticationManager()
  suppressWarnings(authValid <- dataone:::isAuthValid(am, mnKNB))
  if (authValid) {
    if(dataone:::getAuthMethod(am, mnKNB) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
  }
  
  suppressWarnings(bagitFile <- getPackage(mnKNB, id=resMapPid))
  expect_true(!is.null(bagitFile))
  expect_true(file.exists(bagitFile))
  
  td <- tempdir()
  suppressWarnings(bagitFile <- getPackage(mnKNB, id=resMapPid, dirPath=td))
  expect_true(!is.null(bagitFile))
  expect_true(file.exists(bagitFile))
  
  # Now check error handling
  # Can't be a valid pid because we just created the unique string.
  #notApid <- sprintf("urn:uuid:%s", UUIDgenerate())
  #err <- try(bagitFile <- getPackage(mn, id=notApid), silent=TRUE)
  #expect_match(class(err), "try-error")
  
})

test_that("updateSystemMetadata() works",{
  # This test requires valid DataONE user authentication and writes to unstable development machines
  skip_on_cran()
  library(dataone)
  # Create a csv file for the data object
  testdf <- data.frame(x=1:10,y=11:20)
  csvfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  write.csv(testdf, csvfile, row.names=FALSE)
  #mnId <- "urn:node:mnStageUCSB2"
  #d1c <- D1Client("STAGING", mnId)
  am <- AuthenticationManager()
  suppressWarnings(authValid <- dataone:::isAuthValid(am, d1cTest@mn))
  if (authValid) {
    if(dataone:::getAuthMethod(am, d1cTest@mn) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
    subject <- dataone:::getAuthSubject(am, d1cTest@mn)
    do1 <- new("DataObject", format="text/csv", user=subject, filename=csvfile)
    # Set replication off, to prevent the bug of serialNumber increasing due to replication bug
    uploadDataObject(d1cTest, do1, replicate=FALSE, public=TRUE)
    id1 <- getIdentifier(do1)
    md1 <- getSystemMetadata(d1cTest@mn, id1)
    expect_false(is.null(md1))
    id <- "uid=jsmith,o=NCEAS,dc=ecoinformatics,dc=org"
    md1 <- addAccessRule(md1, id, "read")
    expect_true(updateSystemMetadata(d1cTest@mn, md1@identifier, md1))
    md1New  <- getSystemMetadata(d1cTest@mn, id1)
    # Did the access policy get updated?
    ap <- md1New@accessPolicy
    expect_true(ap[ap$subject==id,"subject"] == id)
  } else {
      skip("This test requires valid authentication.")
  }
})

test_that("MNode updateObject() using dataobj argument", {
    # This test requires valid DataONE user authentication and writes to unstable development machines
    skip_on_cran()
    library(dataone)
    library(digest)
    library(datapack)
    library(uuid)
    library(XML)
    #cn <- CNode("SANDBOX")
    #cn <- CNode("DEV2")
    #cn <- CNode("STAGING")
    # mnDemo1 is api v1 on 20151208, but that could change
    # Use this v1 node to test with both a current token available
    # and a certificate.
    #mnId <- "urn:node:mnSandboxUCSB2"
    #mnId <- "urn:node:mnDevUCSB2"
    #mnId <- "urn:node:mnStageUCSB2"
    #mn <- getMNode(cn, mnId)
    am <- AuthenticationManager()
    # Suppress openssl, cert missing warnings
    suppressMessages(authValid <- dataone:::isAuthValid(am, mnTest))
    if (authValid) {
        if(dataone:::getAuthMethod(am, mnTest) == "cert" && grepl("apple-darwin", sessionInfo()$platform)) skip("Skip authentication w/cert on Mac OS X")
        user <- dataone:::getAuthSubject(am, mnTest)
        newid <- generateIdentifier(mnTest, "UUID")

        # Create a data object, and convert it to csv format
        testdf <- data.frame(x=1:10,y=11:20)
        csvfile <- paste(tempfile(), ".csv", sep="")
        write.csv(testdf, csvfile, row.names=FALSE)
        
        # Create SystemMetadata for the object
        format <- "text/csv"
        size <- file.info(csvfile)$size
        sha256 <- digest(csvfile, algo="sha256", serialize=FALSE, file=TRUE)
        # specify series id for this sysmeta. This will only be used if uploading to a DataONE v2 node
        seriesId <- UUIDgenerate()
        sysmeta <- new("SystemMetadata", identifier=newid, formatId=format, size=size, checksum=sha256,
                       originMemberNode=mnTest@identifier, authoritativeMemberNode=mnTest@identifier, seriesId=seriesId)
        sysmeta <- addAccessRule(sysmeta, "public", "read")
        
        
        # Upload the data to the MN using createObject()
        createdId <- createObject(mnTest, newid, csvfile, sysmeta)
        
        # Update the object with a new version
        updateid <- generateIdentifier(mnTest, "UUID")
        d1test <- D1Client(cnStaging, mnTest)
        dataObject <- getDataObject(d1test, createdId)
        dataObject@sysmeta@identifier <- updateid
        
        newId <- updateObject(mnTest, 
                              pid = createdId, 
                              newpid = updateid,
                              sysmeta = dataObject@sysmeta,
                              dataobj = dataObject@data)
        expect_false(is.null(newId))
        expect_match(newId, updateid)
        updsysmeta <- getSystemMetadata(mnTest, updateid)
        expect_match(class(updsysmeta)[1], "SystemMetadata")
        expect_match(updsysmeta@obsoletes, newid)
    }    
    else {
        skip("This test requires valid authentication.")
    }
})