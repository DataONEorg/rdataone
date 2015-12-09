context("AuthenticationManager tests")

test_that("AuthenticationManager isAuthValid() works", {
  am <- AuthenticationManager()
  expect_false(is.null(am))
  cn <- CNode("SANDBOX")
  mn <- getMNode(cn, "urn:node:mnSandboxUCSB2")
  skip_on_cran()
  expect_true(isAuthValid(am, mn), info="Tests require a valid authentication token or X.509 certificate")
  
  cn <- CNode("SANDBOX2")
  mnId <- "urn:node:mnDemo1"
  mn <- getMNode(cn, mnId)
  expect_true(isAuthValid(am, mn), info=sprintf("Tests require a valid authentication for node %s with api version %s", mnId, mn@APIversion))
})

test_that("AuthenticationManager getAuthMethod(), getAuthToken(), getCert() work", {
  am <- AuthenticationManager()
  expect_that(is.null(cm), is_false())
  expect_false(is.null(am))
  skip_on_cran()
  cn <- CNode("SANDBOX")
  mn <- getMNode(cn, "urn:node:mnSandboxUCSB2")
  expect_true(isAuthValid(am, mn), info="Tests require a valid authentication token or X.509 certificate")
  expect_false(is.na(getAuthMethod(am)))
  if(getAuthMethod(am) == "token") {
    expect_false(is.na(getAuthToken(am)))
  } else {
    expect_false(is.na(getCert(am)))
    expect_true(file.exists(getCert(am)))
  }
})

test_that("getAuthExpires() works", {
  am <- AuthenticationManager()
  expect_that(is.null(cm), is_false())
  expect_false(is.null(am))
  cn <- CNode("SANDBOX")
  mn <- getMNode(cn, "urn:node:mnSandboxUCSB2")
  skip_on_cran()
  expect_true(isAuthValid(am, mn), info="Tests require a valid authentication token or X.509 certificate")
  expect_false(is.na(getAuthMethod(am)))
  # TODO: Check auth token expiration when R JWT package is available.
  if(getAuthMethod(am) == "token") {
    expect_false(is.na(getAuthToken(am)))
  } else {
    expect_false(is.na(getCert(am)))
    expect_false(isAuthExpired(am))
  }
})

test_that("isCertExpired() works", {
  skip_on_cran()
  am <- AuthenticationManager()
  cn <- CNode("SANDBOX")
  expect_true(isAuthValid(am, cn), info="Tests require a valid authentication token or X.509 certificate")
  expDate <- getAuthExpires(am)
  # TODO: check token exp date when JWT available.
  ct <- format(Sys.time(), format="%FT%H:%M:%SZ", tz="UTC")
  if(!is.na(expDate)) {
    if(isAuthExpired(am)) {
      expect_true(expDate > ct) 
    } else {
      expect_false(expDate < ct) 
    }
  }
})

test_that("getAuthSubject() works", {
  skip_on_cran()
  am <- AuthenticationManager()
  cn <- CNode("SANDBOX")
  authValid <- isAuthValid(am, cn)
  expect_true(authValid, info="Tests require a valid authentication token or X.509 certificate")
  result <- getAuthSubject(am)
  if (!authValid) {
    # No valid authentication, so should return "public" user
    expect_that(result, matches("public"))
  } else {
    # Testing normal case
    expect_that(length(result) > 0, is_true())
  }
})

test_that("obscureAuth(), restoreAuth() work", {
  skip_on_cran()
  am <- AuthenticationManager()
  cn <- CNode("SANDBOX")
  expect_true(isAuthValid(am, cn), info="Tests require a valid authentication token or X.509 certificate")
  obscureAuth(am)
  expect_false(isAuthValid(am, cn))
  expect_equal(getAuthSubject(am), "public")
  expect_equal(getAuthToken(am), as.character(NA))
  expect_equal(getCert(am), as.character(NA))
  restoreAuth(am)
  expect_true(isAuthValid(am, cn))
  if(getAuthMethod(am) == "token") {
    expect_false(identical(getAuthToken(am), as.character(NA)))
  } else {
    expect_false(identical(getCert(am), as.character(NA)))
  }
})

