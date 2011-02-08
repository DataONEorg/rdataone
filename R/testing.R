d1.t1 <- function() {
   CN_URI <- "http://cn.dataone.org/cn/"
   id <- "jones.357.4"
   d1 <- D1Client(CN_URI)
   dp <- getD1Object(d1, id)
   print(c("Count of data objects: ", getDataCount(dp)))
   mydf <- asDataFrame(dp,1)
   print(summary(mydf))
}

d1.go <- function() {
   #uri <- "http://cn-dev.dataone.org/cn/"
   uri <- "http://cn.dataone.org/cn/"
   #id <- "pisco_subtidal.82.1"
   #id <- "kgordon.22.3"
   #id <- "kgordon.21.3"
   #id <- "connolly.260.1"
   #id <- "kgordon.25.3"
   #id <- "knb:testid:2010302125029284"
   #id <- "repl:testID20113514424239"
   id <- "knb:testid:201135125627107"
   d1 <- D1Client(uri)
   dp <- getPackage(d1, id)
   print(c("Count of data objects: ", getDataCount(dp)))
   mydf <- getData(dp,1)
   print(summary(mydf))
}

a.kgordon <- function(mydf) {
   plot_colors <- c("blue","red","forestgreen")
   plot(Biomass ~ Density, data=mydf, col=plot_colors[2])
   boxplot(Density ~ Taxon, data=mydf, col=plot_colors[1], outline=F)
   title(xlab="Taxon Number")
   title(ylab="Density")
}

d1.test <- function() {
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

d1.get <- function(identifier) {
   nodeurl <- "http://localhost:8080/knb/"
   username <- "uid=kepler,o=unaffiliated,dc=ecoinformatics,dc=org"
   pw <- "kepler"

   cli <- .jnew("org/dataone/client/D1Client", nodeurl) 
   token <- d1.login(username, pw)
   guid <- .jnew("org/dataone/service/types/Identifier")
   .jcall(guid, "V", "setValue", identifier)
   istream <- cli$get(token, guid) 
   .jcheck(silent = FALSE)
   iou <-  .jnew("org/apache/commons/io/IOUtils") 
   .jcheck(silent = FALSE)
   rdata <- iou$toString(istream)
   .jcheck(silent = FALSE)
   return(rdata)
}

d1.login <- function(username, pw) {
   nodeurl <- "http://localhost:8080/knb/"
   cli <-  .jnew("org/dataone/client/D1Client", nodeurl) 
   token <- cli$login(username, pw) 
   .jcheck(silent = FALSE)
   return(token)
}

d1.hello <- function(){
   hjw <- .jnew("HelloJavaWorld") # create instance of HelloJavaWorld class
   out <- .jcall(hjw, "S", "sayHello") # invoke sayHello method
   return(out)
}

d1.javaversion <- function(){
   sys <- .jnew("java/lang/System")
   print(.jcall(sys, "S", "getProperty", "java.version"))
   print(.jcall(sys, "S", "getProperty", "java.runtime.version"))
}

d1.cp <- function(){
   cp <- .jclassPath()
   return(cp)
}

d1.inttest <- function(){
   myint <- .jnew("java/lang/Integer", "7")
   value <- .jcall(myint, "I", "intValue")
   return(value)
}

