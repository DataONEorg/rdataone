library(digest)
library(uuid)

# Read in species observation data
speciesData <- read.csv(system.file("extdata/sample.csv", package="dataone"))

# Very simple filter to extract only the species of interest
speciesData <- speciesData[speciesData$species=="MUSS",]
outfile <- tempfile(pattern="filteredSpecies", fileext=".csv")
write.csv(speciesData, file=outfile, row.names=FALSE)
