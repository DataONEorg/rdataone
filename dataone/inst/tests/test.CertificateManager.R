context("CertificateManager tests")

test_that("CertificateManager loads", {
  cm <- CertificateManager()
  expect_that(is.null(cm), is_false())
  jclassName <- cm@jClientIdManager@name
  expect_that(jclassName,matches("org.dataone.client.auth.ClientIdentityManager"))
})

test_that("getCertExpires", {
    cm <- CertificateManager()
    expires <- getCertExpires(cm)
    if (is.null(expires)) {
        ## if no certificate is installed, this is the correct answer
        expect_that(expires, equals(NULL))
    } else {
        ## need to see a date string
    }
})

test_that("isCertExpired", {
    cm <- CertificateManager()
    isExpired <- isCertExpired(cm)
    if (is.null(getCertExpires(cm))) {
        ## if no certificate is installed, then should equal FALSE
        ## TODO: Check this logic, seems suspect to me, but transfered from old test
        expect_that(isExpired, is_false())
    } else {
        ## TODO: finish the logic here
    }
})

test_that("showClientSubject", {
    cm <- CertificateManager()
    result <- showClientSubject(cm)
    expires <- getCertExpires(cm)
    if (is.null(expires)) {
        # Testing no certificate case
        expect_that(result, matches("public"))
    } else if (isCertExpired(cm)) {
        # Testing expired certificate case
        expect_that(result, matches("[EXPIRED]"))
    } else {
        # Testing normal case
        expect_that(length(result) > 0, is_true())
    }
})

test_that("obscureCert and restoreCert", {
    cm <- CertificateManager()
    subject1 <- showClientSubject(cm)
    if (subject1 == "public") {
        expect_that(obscureCert(cm), throws_error())
    } else {
        obscureCert(cm)
        subject2 <- showClientSubject(cm)
        expect_that(subject2, matches("public"))
        restoreCert(cm)
        subject3 <- showClientSubject(cm)
        expect_that(subject3, matches(subject1))
    }
})
