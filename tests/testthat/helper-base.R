# Reduce redundant calls to the same service - this only needs to be made
# once per test 
cnProd <- CNode()
d1cKNB <- D1Client("PROD", "urn:node:KNB")
mnKNB <- d1cKNB@mn
d1cProd <- D1Client("PROD")
d1cTestKNB <- D1Client("STAGING2", "urn:node:mnTestKNB")
# If mnStageUCSB2 isn't available for write tests, use mnTestKNB
#d1cTest <- D1Client("STAGING", "urn:node:mnStageUCSB2")
d1cTest <- d1cTestKNB
mnTest <- d1cTestKNB@mn
