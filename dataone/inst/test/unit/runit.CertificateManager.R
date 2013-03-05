


test.CertificateManager <- function() {
    cm <- CertificateManager()
    jclassName <- cm@jClientIdManager@name
    checkEquals(jclassName,"org.dataone.client.auth.ClientIdentityManager")
}

test.getCertExpires <- function() {
    cm <- CertificateManager()
    expires <- getCertExpires(cm)
    if (is.null(expires)) {
        ## if no certificate is installed, this is the correct answer
        checkEquals(expires, NULL)
    } else {
        ## need to see a date string
    }
}

test.isCertExpired <- function() {
    cm <- CertificateManager()
    isIt <- isCertExpired(cm)
    if (is.null(getCertExpires(cm))) {
        ## if no certificate is installed, then should equal FALSE
        checkTrue(!isIt)
    } else {
        ## TODO: finish the logic here
        checkTrue
    }
}

test.showClientSubject <- function() {
    cm <- CertificateManager()
    result <- showClientSubject(cm)
    expires <- getCertExpires(cm)
    if (is.null(expires)) {
        message("testing no certificate case")
        checkEquals(result,"public")
    } else if (isCertExpired(cm)) {
        message("testing expired certificate case")
        checkTrue(starts.with(result,"[EXPIRED]"))
    } else {
        message("testing normal case")
        checkTrue(length(result) > 0)
    }
}

test.obscureCert <- function() {
    cm <- CertificateManager()
    result <- showClientSubject(cm)
    if (result == "public") {
        checkException(obscureCert(cm))
    } else {
        obscureCert(cm)
        result <- showClientSubject(cm)
        checkEquals(result, "public")
        restoreCert(cm)
    }
}

test.restoreCert <- function() {
    cm <- CertificateManager()
    startSubject <- showClientSubject(cm)
    if (startSubject != "public") { 
        obscureCert(cm)
    }
    intermedSubject <- showClientSubject(cm)
    checkEquals(intermedSubject,"public")
    restoreCert(cm)
    endSubject <- showClientSubject(cm)
    checkEquals(startSubject,endSubject)
}