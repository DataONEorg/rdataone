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
  cn_env <- Sys.getenv("CN_ENV")
  if(cn_env == "") cn_env <- "DEV"
  if(cn_env != "DEV") print(paste("** Using the", cn_env, "environment."))
  mn_nodeid <- Sys.getenv("MN_NODE_ID")
  sleep_seconds <- 200

  d1.set_certificate_location()

  # Make sure the environment is sane.  Set the environment variable 
  #   SKIP_JAVAENV_TEST="skip" in ${HOME}/.Renviron to suppress.
  if ("skip" != Sys.getenv("SKIP_JAVAENV_TEST"))  d1.testJavaEnvironment(cn_env)


  print(" ")
  print("####### Start Testing ######################")

  objId <- ""
  objId <- d1.testCreateDataObject(cn_env, mn_nodeid)
  # d1.testCreateEMLObject(cn_env, mn_nodeid)
  d1.testConvertCSV(cn_env)

  # Pause to wait for the CN to sync with the MN  (
  if(objId != "") {
    print(paste("Waiting", sleep_seconds,
		"seconds to let the CN sync the EML object."))
    Sys.sleep(sleep_seconds)
  } else {
    objId <- "doi:10.6085/AA/MORXXX_015MTBD009R00_20080411.50.1"
  }

  # d1.getD1Object(cn_env, objId)
  # d1.getPackage(cn_env)

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

  # Create a permanent id.
  cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
  id <- paste("r_test1", cur_time, "1", sep=".")
  #print(paste("id: ", id))

  # Create a DataONE client
  d1_client <- D1Client(env, mn_nodeid)

  # Create a data table, and write it to csv format
  testdf <- data.frame(x=1:10,y=11:20)
  #print(paste("testdf: ", testdf))
  csvdata <- convert.csv(d1_client, testdf)
  format <- "text/csv"

  # Create a D1Object for the table, and upload it to the MN
  d1_object <- createD1Object(id, csvdata, format, mn_nodeid)
  if(is.jnull(d1_object)) {
    print("d1_object is null")
    return(NULL)
  }

  #print(d1_object$getData())
  newId <- d1_object$getIdentifier()
  #print(paste("ID of d1_object:",newId$getValue()))
  d1_object$setPublicAccess(d1_client@session)

  #print("Attempting d1_client@create()")
  create(d1_client, d1_object)
  if (!is.null(e <- .jgetEx())) {
    print("Java exception was raised")
    print(.jcheck(silent=TRUE))
  }
  #print("Finished object upload.")

  print("Test 1: finished")
  return(id)
}

#+----------------------------------------------------------------------+#
#|									|#
#|	Create a basic DataONE EML D1Object.				|#
#|									|#
#+----------------------------------------------------------------------+#
d1.testCreateEMLObject <- function(env, mn_nodeid) {
  print(" ")
  print("####### Test 2: createD1Object for EML ######################")

  # Create new id.
  cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
  id <- paste("r_test2", cur_time, "1", sep=".")

  # Read a text file from disk
  libPath <- .libPaths()
  tfiles.directory <- file.path(libPath[1], .packageName, "testfiles",
			        fsep=.Platform$file.sep)
  filenames <- list.files(tfiles.directory)
  tfname <- file.path(tfiles.directory, filenames[[1]],
		      fsep=.Platform$file.sep)
  info <- file.info(tfname)
  doc_char <- readChar(tfname, info$size)
  format <- "eml://ecoinformatics.org/eml-2.1.0"

  # Create a D1Object for the table, and upload it to the MN
  d1object <- createD1Object(id, doc_char, format, mn_nodeid)
  #print(d1object$getData())
  newId <- d1object$getIdentifier()
  print(paste("ID of d1object:", newId$getValue()))

  # Create a DataONE client.
  d1_client <- D1Client(env, mn_nodeid)

  # Upload object.
  d1object$setPublicAccess(d1_client@session)
print("creating object")
  create(d1_client, d1object)
  .jcheck(silent = FALSE)
  print("Finished object upload.")

  print("Test 2: finished")
}

#+----------------------------------------------------------------------+#
#|									|#
#|	Convert a dataframe to a csv stream.				|#
#|									|#
#+----------------------------------------------------------------------+#
d1.testConvertCSV <- function(env) {
  print(" ")
  print("####### Test 3: convert.csv ######################")

  # Create a data table, and convert it to csv format
  testdf <- data.frame(x=1:10,y=11:20)
  print(testdf)

  d1 <- D1Client(env)
  csv <- convert.csv(d1, testdf)
  print(csv)

  print("Test 3: finished")
}

#+----------------------------------------------------------------------+#
#|									|#
#|	Retreive a D1Object from DataONE using the client library.	|#
#|									|#
#+----------------------------------------------------------------------+#
d1.getD1Object <- function(env, id) {
  print(" ")
  print("####### Test 4: getD1Object ######################")

  print(paste("Attempting to get object:", id))
  d1Client <- D1Client(env)
  d1Object <- getD1Object(d1Client, id)
  .jcheck(silent = FALSE)

  databytes <- d1Object$getData()
  jString <- .jnew("java/lang/String", databytes)
  .jcheck(silent = FALSE)
  data_size <- jString$length()
  if(data_size < 14000) {
    print(paste("Data is too small (expecting at least 14000, only found",
                data_size))
  }

  print("Test 4: finished")
}

#+----------------------------------------------------------------------+#
#|									|#
#|	Create a basic DataONE D1Object.				|#
#|									|#
#+----------------------------------------------------------------------+#
d1.getPackage <- function(env) {
  print(" ")
  print("####### Test 5: getPackage ######################")

  # Get the CN (only) client.
  d1Client <- D1Client(env)

  # Make sure this doesn't work
  dp <- getPackage(d1Client, "doi:10.6085/AA/MORXXX_015MTBD009R00_20080411.50.1")
  if(!is.null(dp)) {
    print("FAIL: Created package out of incorrect format type")
    return
  }

  # Now, get the package.
  id <- "resourceMap_dpennington.121.2"
  print(" ")
  print(paste("Getting object with ID:", id))

  rDataPackage <- getPackage(d1Client, id)
  if(is.null(rDataPackage)) {
    print("FAIL: Couldn't create package")
    return
  }

  databytes <- getData(rDataPackage, "doi:10.5063/AA/IPCC.200802022123018.1")
  if(is.null(databytes)) {
  print("FAIL: Couldn't get data")
  return
  }

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
  print(" ")
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

