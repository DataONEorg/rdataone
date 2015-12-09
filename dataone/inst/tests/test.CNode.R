context("CNode tests")
test_that("dataone library loads", {
	library(dataone)
})
test_that("CNode constructors", {
	library(dataone)
	cn <- CNode()
	expect_that(cn@endpoint, matches("https://cn.dataone.org/cn"))
	cn <- CNode("PROD")
	expect_that(cn@endpoint, matches("https://cn.dataone.org/cn"))
	cn <- CNode("DEV")
	expect_that(cn@endpoint, matches("https://cn-dev.test.dataone.org/cn"))
	cn <- CNode("STAGING2")
	expect_that(cn@endpoint, matches("https://cn-stage-2.test.dataone.org/cn"))
})
test_that("CNode listNodes()", {
  library(dataone)
  cn <- CNode("PROD")
  nodelist <- listNodes(cn)
  expect_that(length(nodelist) > 0, is_true())
  expect_that(class(nodelist[[1]]), matches("Node"))
  expect_that(nodelist[[1]]@identifier, matches("urn:node:"))
  expect_that(nodelist[[1]]@type, matches("cn|mn"))
  expect_that(nodelist[[1]]@state, matches("up"))
  expect_that(nodelist[[length(nodelist)]]@identifier, matches("urn:node:"))
  expect_that(nodelist[[length(nodelist)]]@baseURL, matches("http"))
  expect_that(nodelist[[length(nodelist)]]@subject, matches("urn:node:"))
  expect_that(nodelist[[length(nodelist)]]@type, matches("cn|mn"))
})

test_that("CNode get()", {
  library(dataone)
  cn <- CNode()
  pid <- "aceasdata.3.2"
  obj <- get(cn, pid)
  xml <- xmlParseDoc(rawToChar(obj), asText=TRUE)
  cname <- class(xml)[1]
  expect_that(cname, matches("XML"))
  chksum <- getChecksum(cn, pid)
  expect_that(chksum, is_a("character"))
  expect_false(is.null(chksum))
})

test_that("CNode getSystemMetadata()", {
  library(dataone)
  cn <- CNode()
  pid <- "aceasdata.3.2"
  sysmeta <- getSystemMetadata(cn, pid)
  expect_that(sysmeta@identifier, matches(pid))
})

test_that("CNode describe()", {
  library(dataone)
  cn <- CNode("STAGING2")
  pid <- "aceasdata.3.2"
  res <- dataone::describe(cn, pid)
  expect_is(res, "list")
  expect_that(res$`content-type`, matches("text/xml"))
})

test_that("CNode getMNode()", {
  library(dataone)
  cn <- CNode()
  nodelist <- listNodes(cn)
  nodeid <- nodelist[[length(nodelist)]]@identifier
  newnode <- getMNode(cn, nodeid)
  expect_that(class(newnode), matches("Node"))
  expect_that(newnode@identifier, matches("urn:node:"))
  expect_that(newnode@type, matches("cn|mn"))
  expect_that(newnode@baseURL, matches("http"))
  expect_that(newnode@subject, matches("urn:node:"))
  newnode <- getMNode(cn, "NOT_A_NODE_ID")
  expect_that(newnode, is_a("NULL"))
})

test_that("CNode resolve()",{
  library(dataone) 
  cn <- CNode()
  id <- "0d7d8e0e-93f5-40ab-9916-501d7cf93e15"
  res <- resolve(cn,id)
  expect_that(res$id, matches(id) )
  expect_that(typeof(res$data),matches("list") )
})

test_that("CNode reserveIdentifier(), hasReservation() works",{
  skip_on_cran()
  library(dataone)
  library(uuid)
  cn <- CNode("SANDBOX")
   
  # For hasReservation(), we have to use the same subject that is in the authorization token or X.509 certificate.
  # Until the dataone package can decrypt auth tokens, we have to manually provide same subject
  # used by reserveIdentifier.  
  am <- AuthenticationManager()
  if (!isAuthValid(am, cn)) {
    stop(sprintf("Valid DataONE authentication is required for this test."))
  }
  
  subject <- getAuthSubject(am)
  # If subject isn't available from the current authentication method, then try
  # the session configuration.
  if (is.na(subject)) {
    subject <- getOption("subject_dn")
    # If session config doesn't have subject_dn set, then use the failback DN
    if (is.null(subject)) {
      warning(sprintf("This test requires setting the \"subject_dn\" option. Please see \"dataone-overview\" vignette."))
      skip("Need to set subject_dn to run this test.")
    }
  }
  
  myId <- sprintf("urn:uuid:%s", UUIDgenerate())
  # researveIdentifier will create the reservation using only the client subject from
  # the current authentication method - either auth token or certificate. 
  newId <- reserveIdentifier(cn, myId)
  expect_equal(myId, newId)

  hasRes <- hasReservation(cn, newId, subject=subject)
  expect_true(hasRes, info=sprintf("Didn't find reserved identifier %s", myId))
})

test_that("CNode listFormats, getFormat",{
  library(dataone) 
  cn <- CNode()
  fmts <- listFormats(cn)
  expect_that(is.data.frame(fmts),is_true())
  expect_that(length(grep("eml", fmts$ID)), is_more_than(0))
  # CHeck that the name returned by getFormat matches the name
  # requested, and in the data.frame from listFormats
  for (i in 1:length(fmts)) {
    thisFmt <- getFormat(cn, fmts[i,'ID'])
    expect_match(fmts[i,'Name'], thisFmt$name)
  }
})
