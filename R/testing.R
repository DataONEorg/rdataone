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



#+----------------------------------------------------------------------+#
#|									|#
#|	appears to be the  main function                                |# 
#|              ( d1.test() is called by install.sh )                   |#
#|									|#
#+----------------------------------------------------------------------+#
d1.test <- function() {

  ## Configurable settings for these tests
  cn_env <- Sys.getenv("CN_ENV")

  if(cn_env == "") cn_env <- "DEV"
  if(cn_env != "DEV") print(paste("** Using the", cn_env, "environment."))

  mn_nodeid <- Sys.getenv("MN_NODE_ID")
  if (mn_nodeid =="") 
     mn_nodeid <- "urn:node:mnDemo5"

  sleep_seconds <- 200

 
  # Check if the certificate is specified.
  d1.set_certificate_location()

  # Make sure the environment is sane.  Set the environment variable 
  #   SKIP_JAVAENV_TEST="skip" in ${HOME}/.Renviron to suppress.
  if ("skip" != Sys.getenv("SKIP_JAVAENV_TEST"))  d1.testJavaEnvironment(cn_env)


  
  print(" ")
  print("####### Start Testing ######################")

#  d1.testGetPackage(cn_env, " ")
#  return()
  
  objId <- ""
  objId <- d1.testCreateDataObject(cn_env, mn_nodeid)
  print(paste("testCreateDataObject created object with id ", objId))

  d1.testConvertCSV(cn_env)

  testPackage <- ""
  testPackage <- d1.testCreateDataPackage(cn_env, mn_nodeid)

  # Pause to wait for the CN to sync with the MN  (
  if(objId != "") {
    print(paste("Waiting", sleep_seconds,
		"seconds to let the CN sync the created object and package"))
    Sys.sleep(sleep_seconds)
  }
  else if (testPackage == "") {
    print(paste("Waiting", sleep_seconds,
		"seconds to let the CN sync the created object and package"))
    Sys.sleep(sleep_seconds)

  }
  else {
    ## what is the logic for this?  is this a known object?
    objId <- "r_test1.2012081311271344882424.1"
  }

  d1.testGetD1Object(cn_env, objId)

  d1.testGetPackage(cn_env, testPackage)

  print("####### End Testing ######################")
}

#+----------------------------------------------------------------------+#
#|									|#
#|	Create a basic DataONE D1Object.				|#
#|									|#
#+----------------------------------------------------------------------+#
d1.testCreateDataObject <- function(env, mn_nodeid) {
  print(" ")
  print("####### Test 1: createD1Object ######################")

  ## Create a permanent id.
  cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
  id <- paste("r_test1", cur_time, "1", sep=".")
  #print(paste("id: ", id))

  ## Create a DataONE client
  d1_client <- D1Client(env, mn_nodeid)

  ## Create a data table, and write it to csv format
  testdf <- data.frame(x=1:10,y=11:20)
  ## print(paste("testdf: ", testdf))
  csvdata <- convert.csv(d1_client, testdf)
  format <- "text/csv"

  ## Build a D1Object for the table, and upload it to the MN
  print("building a D1Object")
  d1Object <- new(Class="D1Object", id, csvdata, format, mn_nodeid)
  if(is.jnull(d1Object)) {
    print("d1Object is null")
    return(NULL)
  }


  pidValue <- getIdentifier(d1Object)
  print(paste("ID of d1Object:",pidValue))

  setPublicAccess(d1Object)

  if (canRead(d1Object,"public")) {
    print("successfully set public access");
  } else {
    print("FAIL: did not set public access");
  }

  print("outputting data bytes...")
  bytes <- getData(d1Object)
  print(bytes)

  print("  Attempting d1_client@create()")
  create(d1_client, d1Object)
  if (!is.null(e <- .jgetEx())) {
    print("Java exception was raised")
    print(.jcheck(silent=TRUE))
  }
  print("Finished object upload.")

  print("Test 1: finished")
  return(id)
}


#+----------------------------------------------------------------------+#
#|									|#
#|	Convert a dataframe to a csv stream.				|#
#|									|#
#+----------------------------------------------------------------------+#
d1.testConvertCSV <- function(env) {
  print(" ")
  print("####### Test 2: convert.csv ######################")

  # Create a data table, and convert it to csv format
  testdf <- data.frame(x=1:10,y=11:20)
  print(testdf)

  d1 <- D1Client(env)
  csv <- convert.csv(d1, testdf)
  print(csv)

  print("Test 2: finished")
}

#+----------------------------------------------------------------------+#
#|									|#
#|	Create a DataONE data package.					|#
#|									|#
#+----------------------------------------------------------------------+#
d1.testCreateDataPackage <- function(env, mn_nodeid) {
  print(" ")
  print("####### Test 3: create DataPackage / EML format  ######################")

  # Get the time for all the later objects.
  cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")

  # Create id's
  package_id <- paste("r_test3", "package", cur_time, sep=".")
  scimeta_id <- paste("r_test3", "scimeta", cur_time, sep=".")
  scidata1_id <- paste("r_test3", "scidata", "1", cur_time, sep=".")
  scidata2_id <- paste("r_test3", "scidata", "2", cur_time, sep=".")

  # Create a DataONE client
  print("@@ testing.R 01: Creating the D1Client...")
  d1_client <- D1Client(env, mn_nodeid)


  #
  # ** Science Metadata Object **
  #

  # Read a text file from disk
  print("@@ testing.R 02: Reading text file from disk...")
  libPath <- .libPaths()
  tfiles.directory <- file.path(libPath[1], .packageName, "testfiles",
			        fsep=.Platform$file.sep)
  filenames <- list.files(tfiles.directory)

  ## this is a bit weak, since it will potentially return the wrong
  ## file if there is any other file in the 'testfiles' directory 
  tfname <- file.path(tfiles.directory, filenames[[1]],
		      fsep=.Platform$file.sep)
  info <- file.info(tfname)
  doc_char <- readChar(tfname, info$size)
  format <- "eml://ecoinformatics.org/eml-2.1.0"


  # Create a D1Object for the table, and upload it to the MN
  print("@@ testing.R 03: Create scimeta object...")
  j_scimeta <- buildD1Object(scimeta_id, doc_char, format, mn_nodeid)
  if(is.jnull(j_scimeta)) {
    print("j_scimeta is null")
    return(NULL)
  }
  newId <- j_scimeta$getIdentifier()
  scimeta_id <- newId$getValue()
  print(paste("ID of scimeta:", scimeta_id))


  #
  # ** Science Data Objects **
  #
  print("@@ testing.R 04: Create first data object...")
  testdf <- data.frame(x=1:10, y=11:20)
  csvdata <- convert.csv(d1_client, testdf)
  format <- "text/csv"
  j_scidata1 <- buildD1Object(scidata1_id, csvdata, format, mn_nodeid)
  if(is.jnull(j_scidata1)) {
    print("j_scidata1 is null")
    return(NULL)
  }
  pid <- j_scidata1$getIdentifier()
  print(paste("ID of data object 1:", pid$getValue()))

  
  print("@@ testing.R 05: Create second data object...")
  testdf <- data.frame(x=21:30, y=31:40, z=41:50)
  csvdata <- convert.csv(d1_client, testdf)
  format <- "text/csv"
  j_scidata2 <- buildD1Object(scidata2_id, csvdata, format, mn_nodeid)
  if(is.jnull(j_scidata2)) {
    print("j_scidata2 is null")
    return(NULL)
  }
  pid <- j_scidata2$getIdentifier()
  print(paste("ID of data object 2:", pid$getValue()))

        
  print("@@ testing.R 06: Create data package...")
  data_package <- DataPackage(package_id)
  print("@@ testing.R 20:  adding metadata...")
  addMeta(data_package, j_scimeta)
  print("@@ testing.R 21:  adding data object 1 ...")
  addData(data_package, j_scidata1)
  print("@@ testing.R 22:  adding data object 2 ...")
  addData(data_package, j_scidata2)

  print("@@ testing.R 23: uploading the object... ")

  # Upload object.
  create(d1_client, data_package)
  print("@@ testing.R 24: checking for call errors...")
  .jcheck(silent = FALSE)
  print("Finished object upload.")

  print("Test 3: finished")
  print(" ")
  return(data_package)
}


#+----------------------------------------------------------------------+#
#|									|#
#|	Retreive a D1Object from DataONE using the client library.	|#
#|									|#
#+----------------------------------------------------------------------+#
d1.testGetD1Object <- function(env, id) {
  print(" ")
  print("####### Test 4: testGetD1Object ######################")

  print(paste("Attempting to get object:", id))
  d1Client <- D1Client(env)
  d1Object <- getD1Object(d1Client, id)
  .jcheck(silent = FALSE)

  databytes <- d1Object$getData()
  jString <- .jnew("java/lang/String", databytes)
  .jcheck(silent = FALSE)
  data_size <- jString$length()
  print(databytes)
#  if(data_size < ) {
#    print(paste("Data is too small (expecting at least 14000, only found",
#                data_size))
#  }

  print("Test 4: finished")
}

#+----------------------------------------------------------------------+#
#|									|#
#|	Create a basic DataONE D1Object.				|#
#|									|#
#+----------------------------------------------------------------------+#
d1.testGetPackage <- function(env, package) {

  # What datapackage?
#  eml_pid <- "knb-lter-gce.187.26"
#  dp_pid <- "resourceMap_jscientist.7.2"
#  dp_scidata_id <- "xx"
#  if(env == "PROD") {
#    eml_pid <- "doi:10.6085/AA/MORXXX_015MTBD009R00_20080411.50.1"
#    dp_pid <- "resourceMap_dpennington.121.2"
#    dp_scidata_id <- "doi:10.5063/AA/IPCC.200802022123018.1"
#  }

  
  print(" ")
  print("####### Test 5: testGetPackage ######################")

  # Get the CN (only) client.
  d1Client <- D1Client(env)

  if (package == " ") {
#    return
    packageId = "r_test3.package.2012102615451351287944"
  }
  else {
    jDP <- package@jDataPackage
    jIdentifier <- jDP$getIdentifier
    packageId <- jIdentifier$getValue()
  }
  # Make sure we can't get a D1Object as a DataPackage


  # Now, get the package.
  print(paste("Getting package with ID:", packageId))

  rDataPackage<- getPackage(d1Client, packageId)
  if(is.null(rDataPackage)) {
    print(paste("FAIL: Created package", packageId, "could not be retrieved"))
    return
  }
  print(" ")

  
    
#  databytes <- getData(rDataPackage, dp_scidata_pid)
#  if(is.null(databytes)) {
#    print("FAIL: Couldn't get data")
#    return
#  }

  # jString <- .jnew("java/lang/String", databytes)
  # .jcheck(silent = FALSE)
  # print(jString)

  print("Test 5: finished")
}


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

d1.set_certificate_location <- function() {
  cert_filename <- Sys.getenv("X509_CERTIFICATE_PATH")
  if(!is.null(cert_filename) && (cert_filename != "")) {
    cert_mgr <- J("org/dataone/client/auth/CertificateManager")$getInstance()
    cert_mgr$setCertificateLocation(cert_filename)
  }
}


#+----------------------------------------------------------------------+#
#|									|#
#|	Verify that the Java environment is working correctly.		|#
#|									|#
#+----------------------------------------------------------------------+#

d1.testJavaEnvironment <- function(env) {
  d1.inttest()
  d1.cp()
  d1.javaversion()
  d1.hello()
  d1.testClientEnv(env)
}

d1.inttest <- function() {
  print("")
  print("####### Test 0.1: rJava Accessible ######################")
  myint <- .jnew("java/lang/Integer", "7")
  value <- .jcall(myint, "I", "intValue")
  print(value)
}

d1.cp <- function() {
  print(" ")
  print("####### Test 0.2: Java Classpath ########################")
  cp <- .jclassPath()
  print(cp)
}

d1.javaversion <- function() {
  print(" ")
  print("####### Test 0.3: Java Version ##########################")
  sys <- .jnew("java/lang/System")
  print(.jcall(sys, "S", "getProperty", "java.version"))
  print(.jcall(sys, "S", "getProperty", "java.runtime.version"))
}

d1.hello <- function() {
  print(" ")
  print("####### Test 0.4: Hello World ###########################")
  hjw <- .jnew("HelloJavaWorld") # create instance of HelloJavaWorld class
  out <- .jcall(hjw, "S", "sayHello") # invoke sayHello method
  print(out)
}

d1.testClientEnv <- function(env) {
  # Create a DataONE client, and set the CN environemnt to use
  print(" ")
  print("####### Test 0.5: testClientEnv  ########################")
  d1 <- D1Client(env)
  print(paste("ENV IS: ", getEndpoint(d1)))
}

