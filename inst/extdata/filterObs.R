library(digest)
library(uuid)

# Read in species observation data
speciesData <- read.csv(system.file("extdata/OwlNightj.csv", package="dataone"))
# Very simple filter to extract entries for 'Strix occidentalis'
varList <- c("countrynum","statenum","Route","RPID","Year","Aou","SpeciesTotal")
strixOccidentalis <- speciesData[speciesData$Aou=="3690", varList]
strixOccidentalis <- strixOccidentalis[order(strixOccidentalis$Year),]
outfile <- sprintf("%s/Strix-occidentalis-obs.csv", tempdir())
write.csv(strixOccidentalis, file=outfile, row.names=FALSE)
