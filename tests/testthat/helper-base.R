# Reduce redundant calls to the same service - this only needs to be made
# once per test 
cnProd <- NULL
cnStaging2 <- NULL
d1cKNB <- NULL
mnKNB <- NULL
d1cProd <- NULL
d1cTestKNB <- NULL
d1cTest <- NULL
mnTest <- NULL
servicesDown <- FALSE

# Define service variables that the checks will use. 
# If a service is down, skip tests
tryCatch({
    cnProd <- CNode()
    cnStaging2 <- CNode("STAGING2")
    d1cKNB <- D1Client("PROD", "urn:node:KNB")
    mnKNB <- d1cKNB@mn
    d1cProd <- D1Client("PROD")
    d1cTestKNB <- D1Client("STAGING2", "urn:node:mnTestKNB")
    # If mnStageUCSB2 isn't available for write tests, use mnTestKNB
    #d1cTest <- D1Client("STAGING", "urn:node:mnStageUCSB2")
    d1cTest <- d1cTestKNB
    mnTest <- d1cTestKNB@mn
}, warning = function(wrn) {
  servicesDown <<- TRUE
}, error = function(err) {
  servicesDown <<- TRUE
})

if(is.null(cnProd)) servicesDown <- TRUE
if(is.null(cnStaging2)) servicesDown <- TRUE
if(is.null(d1cKNB)) servicesDown <- TRUE
if(is.null(mnKNB)) servicesDown <- TRUE
if(is.null(d1cProd)) servicesDown <- TRUE
if(is.null(d1cTestKNB)) servicesDown <- TRUE
if(is.null(d1cTest)) servicesDown <- TRUE
if(is.null(mnTest)) servicesDown <- TRUE