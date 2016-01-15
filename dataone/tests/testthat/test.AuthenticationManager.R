context("AuthenticationManager tests")

test_that("AuthenticationManager isAuthValid() for v2 node works", {
  skip_on_cran()
  am <- AuthenticationManager()
  expect_false(is.null(am))
  # v2 node
  cn <- CNode("STAGING")
  mn <- getMNode(cn, "urn:node:mnStageUCSB2")
  # Suppress PKIplus, cert missing warnings
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, mn)
  options(warn = warnLevel)
  if(!authValid) {
    expect_match(getAuthSubject(am), "public")
  } else {
    expect_match(getAuthMethod(am), "cert|token")
    expect_false(isAuthExpired(am))
  }
})

test_that("AuthenticationManager isAuthValid() for v1 node works", {
  skip_on_cran()
  # MAC OS X currently doesn't ahve a valid authentication mechanism for
  # v1 nodes, for various curl / Mac OS X version combinations, so skip this
  # v1 test for all Mac OS X versions
  if(grepl("apple-darwin", sessionInfo()$platform)) skip("Skip v1 node test on Mac OS X")
  am <- AuthenticationManager()
  expect_false(is.null(am))
  cn <- CNode("STAGING")
  # v1 node
  mn <- getMNode(cn, "urn:node:mnStageUNM1")  # Suppress PKIplus, cert missing warnings
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, mn)
  options(warn = warnLevel)
  if(!authValid) {
    expect_match(getAuthSubject(am), "public")
  } else {
    expect_match(getAuthMethod(am), "cert|token")
    expect_false(isAuthExpired(am))
  }
})

test_that("AuthenticationManager getAuthMethod(), getAuthToken(), getCert() work", {
  am <- AuthenticationManager()
  expect_that(is.null(cm), is_false())
  expect_false(is.null(am))
  skip_on_cran()
  cn <- CNode("STAGING")
  mn <- getMNode(cn, "urn:node:mnStageUCSB2")
  # Suppress PKIplus, cert missing warnings
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, mn)
  options(warn = warnLevel)
  if(authValid) {
    expect_false(is.na(getAuthMethod(am)))
    if(getAuthMethod(am) == "token") {
      expect_false(is.na(getAuthToken(am)))
    } else {
      expect_false(is.na(getCert(am)))
      expect_true(file.exists(getCert(am)))
    }
  }
})

test_that("getAuthExpires() works", {
  skip_on_cran()
  am <- AuthenticationManager()
  expect_that(is.null(cm), is_false())
  expect_false(is.null(am))
  cn <- CNode("STAGING")
  mn <- getMNode(cn, "urn:node:mnStageUCSB2")
  # Suppress PKIplus, cert missing warnings
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, mn)
  options(warn = warnLevel)
  if(authValid) {
    expect_false(is.na(getAuthMethod(am)))
    # TODO: Check auth token expiration when R JWT package is available.
    if(getAuthMethod(am) == "token") {
      expect_false(is.na(getAuthToken(am)))
    } else {
      expect_false(is.na(getCert(am)))
      expect_false(isAuthExpired(am))
      expect_false(is.na(getAuthExpires(am)))
    }
  }
})

test_that("isCertExpired() works", {
  skip_on_cran()
  am <- AuthenticationManager()
  cn <- CNode("STAGING")
  # Suppress PKIplus, cert missing warnings
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, cn)
  options(warn = warnLevel)
  if(authValid) {
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
  }
})

test_that("getAuthSubject() works", {
  skip_on_cran()
  am <- AuthenticationManager()
  cn <- CNode("STAGING")
  # Suppress PKIplus, cert missing warnings
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, cn)
  options(warn = warnLevel)
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
  cn <- CNode("STAGING")
  # Disable authentication
  # Suppress PKIplus, cert missing warnings
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, cn)
  options(warn = warnLevel)
  obscureAuth(am)
  
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, cn)
  options(warn = warnLevel)
  expect_equal(getAuthSubject(am), "public")
  expect_equal(getAuthToken(am), as.character(NA))
  expect_equal(getCert(am), as.character(NA))
  
  restoreAuth(am)
  # Suppress PKIplus, cert missing warnings
  warnLevel <- getOption("warn")
  options(warn = -1)
  authValid <- isAuthValid(am, cn)
  options(warn = warnLevel)
  if(authValid) {
    if(getAuthMethod(am) == "token") {
      expect_false(identical(getAuthToken(am), as.character(NA)))
    } else {
      expect_false(identical(getCert(am), as.character(NA)))
    }
  }
})

