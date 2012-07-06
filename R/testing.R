#
#   This work was created by participants in the DataONE project, and is
#   jointly copyrighted by participating institutions in DataONE. For
#   more information on DataONE, see our web site at http://dataone.org.
#
#     Copyright 2011-2012
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#

d1.test <- function() {
    # Configurable settings for these tests
    cn_env <- "DEV"
    mn_nodeid <- Sys.getenv("MN_NODE_ID")
    sleep_seconds <- 200

    # Make sure the environment is sane.  Set the environment variable 
    #   SKIP_JAVAENV_TEST="skip" in ${HOME}/.Renviron to suppress.
    if ("skip" != Sys.getenv("SKIP_JAVAENV_TEST"))  d1.testJavaEnvironment(cn_env)

    print(" ")
    print("####### Start Testing ######################")
    objId <- d1.testCreateDataObject(cn_env, mn_nodeid)
    d1.testCreateEMLObject(cn_env, mn_nodeid)
    d1.testConvertCSV(cn_env)
    # Pause to wait for the CN to sync with the MN
    Sys.sleep(sleep_seconds)
    d1.getD1Object(cn_env, objId)
    d1.getPackage(cn_env, objId)
    print("####### End Testing ######################")
}

d1.testCreateDataObject <- function(env, mn_nodeid) {
   print(" ")
   print("####### Test 1: createD1Object ######################")
   
   cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
   id <- paste("r:test", cur_time, "1", sep=".")
   #print(paste("id: ", id))
   
   # Create a DataONE client, and login
   d1Client <- D1Client(env)

   # Create a data table, and write it to csv format
   testdf <- data.frame(x=1:10,y=11:20)
   #print(paste("testdf: ", testdf))
   csvdata <- convert.csv(d1Client, testdf)
   format <- "text/csv"

   # Create a D1Object for the table, and upload it to the MN
   d1object <- createD1Object(D1Object(), id, csvdata, format, mn_nodeid)
   #print(d1object$getData())
   newId <- d1object$getIdentifier()
   print(paste("ID of d1object:",newId$getValue()))
   d1object$setPublicAccess(d1Client@session)

print("Attempting to create object.")
   d1object$create(d1Client@session)
   if (!is.null(e<-.jgetEx())) {
       print("Java exception was raised")
       print(.jcheck(silent=TRUE))
   }
   print("Finished object upload.")
   print("Test finished")
   return(id)
}

d1.testCreateEMLObject <- function(env, mn_nodeid) {
   print(" ")
   print("####### Test 2: createD1Object for EML ######################")
   cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
   id <- paste("r:test", cur_time, "1", sep=".")
   
   # Create a DataONE client, and login
   d1 <- D1Client(env)

   # Read a text file from disk
   libPath <- .libPaths()
   tfiles.directory <- file.path(libPath[1], .packageName, "testfiles", fsep=.Platform$file.sep)
   filenames <- list.files(tfiles.directory)
   tfname <- file.path(tfiles.directory, filenames[[1]], fsep=.Platform$file.sep)
   info <- file.info(tfname)
   doc_char <- readChar(tfname, info$size)
   format <- "eml://ecoinformatics.org/eml-2.1.0"

   # Create a D1Object for the table, and upload it to the MN
   d1object <- createD1Object(d1, id, doc_char, format, mn_nodeid)
   #print(d1object$getData())
   newId <- d1object$getIdentifier()
   print("ID of d1object:")
   print(newId$getValue())
   d1object$setPublicAccess(d1@session)
   print("Finished setting access.")
   d1object$create(d1@session)
   .jcheck(silent = FALSE)
   print("Finished object upload.")
   print("Test finished")
}

d1.testConvertCSV <- function(env) {
   print(" ")
   print("####### Test 3: convert.csv ######################")
   ##selectCN()
   d1 <- D1Client(env)
   # Create a data table, and convert it to csv format
   testdf <- data.frame(x=1:10,y=11:20)
   print(testdf)
   csv <- convert.csv(d1, testdf)
   print(csv)
}

d1.getD1Object <- function(env, id) {
   print(" ")
   print("####### Test 4: getD1Object ######################")
   #selectCN()
   print(paste("Getting object with ID:", id))
   d1 <- D1Client(env)
   print("D1Client created.")
   dp <- getD1Object(d1, id)
   print("D1Object created.")
   print(c("Count of data objects: ", getDataCount(dp)))
   mydf <- asDataFrame(dp,1)
   print(summary(mydf))
}

d1.getPackage <- function(env, id) {
   print(" ")
   print("####### Test 5: getPackage ######################")
   #selectCN()
   print(paste("Getting object with ID:", id))
   d1 <- D1Client(env)
   dp <- getPackage(d1, id)
   print(c("Count of data objects: ", getDataCount(dp)))
   mydf <- getData(dp,1)
   print(summary(mydf))
}

#selectCN <- function() {
   #CN_URI <- "https://cn-dev-rr.dataone.org/cn"
   #config <- J("org/dataone/configuration/Settings")$getConfiguration()
   #config$setProperty("D1Client.CN_URL", CN_URI)
#}

a.kgordon <- function(mydf) {
   plot_colors <- c("blue","red","forestgreen")
   plot(Biomass ~ Density, data=mydf, col=plot_colors[2])
   boxplot(Density ~ Taxon, data=mydf, col=plot_colors[1], outline=F)
   title(xlab="Taxon Number")
   title(ylab="Density")
}

d1.analyze <- function() {
   print(tapply(bfdata[[1]]$count, bfdata[[1]]$species, mean, na.rm = TRUE))
   print(tapply(bfdata[[1]]$count, bfdata[[1]]$species, sd, na.rm = TRUE))
   print(tapply(bfdata[[1]]$reprod_state, bfdata[[1]]$species, mean, na.rm = TRUE))
   print(tapply(bfdata[[1]]$reprod_state, bfdata[[1]]$species, sd, na.rm = TRUE))
   boxplot(count ~ species, data=bfdata[[1]])
   boxplot(count ~ pisco.code, data=bfdata[[1]])
   boxplot(reprod_state ~ species, data=bfdata[[1]])
   boxplot(reprod_state ~ pisco.code, data=bfdata[[1]])
}


#-- Test the Java environment --------------------------------------------------

d1.testJavaEnvironment <- function(env) {
    d1.inttest()
    d1.cp()
    d1.javaversion()
    d1.hello()
    d1.testClientEnv(env)
}

d1.testClientEnv <- function(env) {
   # Create a DataONE client, and set the CN environemnt to use
   print(" ")
   print("####### Test 0.5: testClientEnv  ######################")
   d1 <- D1Client(env)
   print(paste("ENV IS: ", getEndpoint(d1)))
}

d1.hello <- function() {
   print(" ")
   print("####### Test 0.4: Hello World ######################")
   hjw <- .jnew("HelloJavaWorld") # create instance of HelloJavaWorld class
   out <- .jcall(hjw, "S", "sayHello") # invoke sayHello method
   print(out)
}

d1.javaversion <- function() {
   print(" ")
   print("####### Test 0.3: Java Version ######################")
   sys <- .jnew("java/lang/System")
   print(.jcall(sys, "S", "getProperty", "java.version"))
   print(.jcall(sys, "S", "getProperty", "java.runtime.version"))
}

d1.cp <- function() {
   print(" ")
   print("####### Test 0.2: Java Classpath ######################")
   cp <- .jclassPath()
   print(cp)
}

d1.inttest <- function() {
   print(" ")
   print("####### Test 0.1: rJava Accessible ######################")
   myint <- .jnew("java/lang/Integer", "7")
   value <- .jcall(myint, "I", "intValue")
   print(value)
}

