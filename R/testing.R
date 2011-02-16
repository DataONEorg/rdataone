d1.test <- function() {
    print("####### Start Testing ######################")
    d1.inttest()
    d1.cp()
    d1.javaversion()
    d1.hello()
    d1.t1()
    d1.t2()
    d1.t3()
    d1.t4()
    print("####### End Testing ######################")
}

d1.t1 <- function() {
   print(" ")
   print("####### Test 1: getD1Object ######################")
   CN_URI <- "http://cn-dev.dataone.org/cn/"
   id <- "erd.365.1"
   d1 <- D1Client(CN_URI)
   dp <- getD1Object(d1, id)
   print(c("Count of data objects: ", getDataCount(dp)))
   mydf <- asDataFrame(dp,1)
   print(summary(mydf))
}

d1.t2 <- function() {
   print(" ")
   print("####### Test 2: getPackage ######################")
   uri <- "http://cn-dev.dataone.org/cn/"
   #uri <- "http://cn.dataone.org/cn/"
   id <- "erd.365.1"
   d1 <- D1Client(uri)
   dp <- getPackage(d1, id)
   print(c("Count of data objects: ", getDataCount(dp)))
   mydf <- getData(dp,1)
   print(summary(mydf))
}

d1.t3 <- function() {
   print(" ")
   print("####### Test 3: convert.csv ######################")
   uri <- "http://cn-dev.dataone.org/cn/"
   d1 <- D1Client(uri)
   # Create a data table, and convert it to csv format
   testdf <- data.frame(x=1:10,y=11:20)
   print(testdf)
   csv <- convert.csv(d1, testdf)
   print(csv)
}

d1.t4 <- function() {
   print(" ")
   print("####### Test 4: createD1Object ######################")
   uri <- "http://cn-dev.dataone.org/cn/"
   mn_uri <- "http://knb-test-1.dataone.org/knb/d1"
   #uri <- "http://cn.dataone.org/cn/"
   username <- "uid=kepler,o=unaffiliated,dc=ecoinformatics,dc=org"
   pw <- "kepler"
   cur_time <- format(Sys.time(), "%Y%m%d%H%M%s")
   id <- paste("r:test", cur_time, "1", sep=".")
   
   # Create a DataONE client, and login
   d1 <- D1Client(uri)
   d1 <- login(d1, username, pw, mn_uri)

   # Create a data table, and write it to csv format
   testdf <- data.frame(x=1:10,y=11:20)
   print(testdf)
   csvdata <- convert.csv(d1, testdf)

   # Create a D1Object for the table, and upload it to the MN
   d1object <- createD1Object(d1, id, csvdata)
   print(d1object$getData())
   d1object$create(d1@token)
   print("Finished object upload.")
   d1object$setPublicAccess(d1@token)
   print("Finished setting access.")
   print("Test 3 passed.")
}

a.kgordon <- function(mydf) {
   plot_colors <- c("blue","red","forestgreen")
   plot(Biomass ~ Density, data=mydf, col=plot_colors[2])
   boxplot(Density ~ Taxon, data=mydf, col=plot_colors[1], outline=F)
   title(xlab="Taxon Number")
   title(ylab="Density")
}

d1.test_old <- function() {
   username <- "uid=kepler,o=unaffiliated,dc=ecoinformatics,dc=org"
   pw <- "kepler"
   uri <- "http://localhost:8080/knb/"
   #id <- "knb:testid:201017503651669"
   id <- "knb:testid:2010199125125239"
   print("Start testing")
   d1 <- D1Client(uri)
   d1 <- login(d1, username, pw)
   print(c("TOKEN is: ", d1@token$getToken()))
   print(d1)
   print(c("Endpoint is: ", getEndpoint(d1)))
   dp <- getPackage(d1, id)
   #print(c("Dumped data object: ", dp))
   print(c("Count of data objects: ", getDataCount(dp)))
   bfdata <- getData(dp,1)
   #print(c("Head of first data object: ", head(bfdata[[1]], 15)))
   print(summary(bfdata[[1]]))
   #d1.analyze()
   print("End testing")
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

d1.login <- function(username, pw, mn_uri) {
   print(" ")
   print("####### Test 0.5: Login ######################")
   d1 <- D1Client(uri)
   d1 <- login(d1, username, pw, mn_uri)
   token <- d1@token
   return(d1)
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

